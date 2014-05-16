-module(erollbar_handler).
-behaviour(gen_event).
-include("erollbar.hrl").
-record(state, {access_token :: erollbar:access_token(),
                modules = undefined :: undefined|[module()],
                items = [] :: [term()]|[],
                batch_max :: pos_integer()|undefined,
                time_max :: erollbar:ms()|undefined,
                time_ref :: reference()|undefined,
                endpoint :: binary(),
                info_fun :: erollbar:info_fun(),
                details :: #details{}
               }).

-define(HTTP_TIMEOUT, 5000).

-export([init/1
         ,handle_event/2
         ,handle_call/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3]).

init([AccessToken, Opts]) ->
    TimeMax = proplists:get_value(time_max, Opts),
    TimeRef = if is_integer(TimeMax) ->
                      erlang:send_after(TimeMax, self(), time_max_reached);
                 true -> undefined
              end,
    {ok, #state{access_token=AccessToken,
                modules=proplists:get_value(modules, Opts),
                batch_max=proplists:get_value(batch_max, Opts),
                time_max=TimeMax,
                time_ref=TimeRef,
                info_fun=proplists:get_value(info_fun, Opts),
                endpoint=proplists:get_value(endpoint, Opts),
                details=#details{platform=proplists:get_value(platform, Opts),
                                 environment=proplists:get_value(environment, Opts),
                                 host=proplists:get_value(host, Opts),
                                 branch=proplists:get_value(branch, Opts),
                                 sha=proplists:get_value(sha, Opts)}
               }}.

handle_event({error_report, _, {_, crash_report, _}} = ErrorReport,
             #state{modules=Modules, details=Details}=State) ->
    case maybe_create_item(ErrorReport, Modules, Details) of
        undefined ->
            {ok, State};
        {ok, Item} ->
            State1 = maybe_send_batch(Item, State),
            {ok, State1}
    end;
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, undefined, State}.

handle_info(time_max_reached, #state{time_max=TimeMax}=State) ->
    State1 = send_items(State),
    TimeRef = erlang:send_after(TimeMax, self(), time_max_reached),
    {ok, State1#state{time_ref=TimeRef}};
handle_info(_Info, State) ->
    {ok, State}.

terminate(Reason, #state{time_ref=TimeRef,
                         items=Items,
                         info_fun=InfoFun}) ->
    if is_reference(TimeRef) -> erlang:cancel_timer(TimeRef);
       true -> ok
    end,
    catch info(InfoFun, [{at, terminate}
                         ,{reason, Reason}
                         ,{dropped, length(Items)}]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal
maybe_send_batch(Item, #state{batch_max=BatchMax,
                              items=Items}=State) when length(Items) + 1 >= BatchMax ->
    send_items(State#state{items=[Item | Items]});
maybe_send_batch(Item, #state{items=Items}=State) ->
    State#state{items=[Item | Items]}.

send_items(#state{items=Items, info_fun=InfoFun, endpoint=Endpoint,
                  access_token=AccessToken, details=Details}=State) ->
    Message = create_message(AccessToken, Items, Details),
    case erollbar_utils:request(Endpoint, Message, ?HTTP_TIMEOUT) of
        ok -> ok;
        {error, Code, Body} ->
            info(InfoFun, [{mod, erollbar_handler},
                           {at, send_items},
                           {code, Code},
                           {body, Body}]);
        {error, Reason} ->
            info(InfoFun, [{mod, erollbar_handler},
                           {at, send_items},
                           {reason, Reason}])
    end,
    State#state{items = []}.

create_message(AccessToken, Items, Details) ->
    ItemBlocks = create_items_block(Items, [], Details),
    Message = [{<<"access_token">>, AccessToken},
               {<<"data">>, ItemBlocks}],
    jsx:encode(Message).

create_items_block([], Retval, _) ->
    Retval;
create_items_block([Item|Items], Retval, #details{environment=Environment,
                                                  host=Host,
                                                  branch=Branch,
                                                  sha=Sha}=Details) ->
    ItemBlock = [{<<"environment">>, Environment},
                 {<<"body">>, Item}],
    ItemBlock1 =
        case create_server_block(Host, Branch, Sha) of
            [] ->
                ItemBlock;
            ServerBlock ->
                [{<<"server">>, ServerBlock} | ItemBlock]
        end,
    create_items_block(Items, [ItemBlock1 | Retval], Details).

create_server_block(Host, Branch, Sha) ->
    Block = add_to_block(<<"host">>, Host, []),
    Block1 = add_to_block(<<"branch">>, Branch, Block),
    add_to_block(<<"sha">>, Sha, Block1).

add_to_block(_, undefined, Block) ->
    Block;
add_to_block(Key, Val, Block) ->
    [{Key, Val} | Block].

maybe_create_item(Report, undefined, Details) ->
    erollbar_parser:parse(Report, Details);
maybe_create_item({error_report, _, {_, crash_report, [Report, Neighbors]}} = Report,
                  Modules, Details) ->
    Module = erollbar_parser:initial_call(Report),
    case lists:member(Module, Modules) of
        true ->
            erollbar_parser:parse(Report, Details);
        false ->
            undefined
    end.

info({M, F}, Details) ->
    M:F(Details);
info(InfoFun, Details) ->
    InfoFun(Details).
