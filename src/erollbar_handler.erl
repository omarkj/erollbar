-module(erollbar_handler).
-behaviour(gen_event).

-record(state, {access_token :: erollbar:access_token(),
                modules = undefined :: undefined|[module()],
                items = [] :: [term()]|[],
                platform :: binary()|undefined,
                environment :: binary()|undefined,
                batch_max :: pos_integer()|undefined,
                time_max :: erollbar:ms()|undefined,
                time_ref :: reference()|undefined,
                endpoint :: string(),
                host :: binary()|undefined,
                branch :: binary()|undefined,
                sha :: binary()|undefined,
                info_fun :: erollbar:info_fun()
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
                platform=proplists:get_value(platform, Opts),
                modules=proplists:get_value(modules, Opts),
                batch_max=proplists:get_value(batch_max, Opts),
                environment=proplists:get_value(environment, Opts),
                time_max=TimeMax,
                time_ref=TimeRef,
                info_fun=proplists:get_value(info_fun, Opts),
                endpoint=proplists:get_value(endpoint, Opts),
                host=proplists:get_value(host, Opts),
                branch=proplists:get_value(branch, Opts),
                sha=proplists:get_value(sha, Opts)
               }}.

handle_event({error_report, _, {_, crash_report, _}} = ErrorReport, State) ->
    case maybe_create_item(ErrorReport, State) of
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
    send_items(State#state{items=Items++[Item]});
maybe_send_batch(Item, #state{items=Items}=State) ->
    State#state{items=Items++[Item]}.

send_items(#state{items=[]}=State) ->
    State;
send_items(#state{items=[Item|Items], info_fun=InfoFun, endpoint=Endpoint,
                  access_token=AccessToken, environment=Environment,
                  host=Host, branch=Branch, sha=Sha}=State) ->
    Message = create_message(AccessToken, Environment, Item, Host, Branch, Sha),
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
    send_items(State#state{items=Items}).

create_message(AccessToken, Environment, Item, Host, Branch, Sha) ->
    Message = [{<<"access_token">>, AccessToken},
                {<<"data">>, [{<<"environment">>, Environment},
                              {<<"body">>, Item}]}],
    Message1 =
        case create_server_block(Host, Branch, Sha) of
            [] ->
                Message;
             ServerBlock ->
                Message ++ [{<<"server">>, ServerBlock}]
        end,
    jsx:encode(Message1).

create_server_block(Host, Branch, Sha) ->
    Block = add_to_block(<<"host">>, Host, []),
    Block1 = add_to_block(<<"branch">>, Branch, Block),
    add_to_block(<<"sha">>, Sha, Block1).

add_to_block(_, undefined, Block) ->
    Block;
add_to_block(Key, Val, Block) ->
    Block++[{Key, Val}].

maybe_create_item({error_report, _, {_, crash_report, [Report, Neighbors]}},
                  #state{modules=undefined}=State) ->
    create_body(Report, Neighbors, State);
maybe_create_item({error_report, _, {_, crash_report, [Report, Neighbors]}},
                  #state{modules=Modules}=State) ->
    {Module, _, _} = proplists:get_value(initial_call, Report),
    case lists:member(Module, Modules) of
        true ->
            create_body(Report, Neighbors, State);
        false ->
            undefined
    end.

create_body(Report, _Neighbors, #state{platform=Platform}) ->
    ErrorInfo = proplists:get_value(error_info, Report, []),
    Item = [{<<"trace">>, create_trace(ErrorInfo)},
            {<<"level">>, <<"error">>},
            {<<"timestamp">>, unix_timestamp()},
            {<<"platform">>, erollbar_utils:to_binary(Platform)},
            {<<"language">>, <<"erlang">>}],
    {ok, Item}.

create_trace({_Error, Reason, Frames}) ->
    [{<<"frames">>, create_frames(Frames, [])},
     {<<"exception">>, [{<<"class">>, erollbar_utils:to_binary(Reason)}]}].

create_frames([], Retval) ->
    Retval;
create_frames([{Module, Fun, Arity, Info}|Rest], Retval) ->
    Filename = case Module:module_info(compile) of
                   undefined ->
                       Module;
                   CompileInfo ->
                       proplists:get_value(source, CompileInfo)
               end,
    Frame = [{<<"filename">>, erollbar_utils:to_binary(Filename)},
             {<<"method">>, iolist_to_binary([erollbar_utils:to_binary(Fun),
                                              <<"/">>, erollbar_utils:to_binary(Arity)])}],
    Frame1 = add_lineno(Frame, proplists:get_value(line, Info)),
    create_frames(Rest, Retval ++ [Frame1]).

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    erollbar_utils:to_binary(Mega*1000000 + Secs).

info(InfoFun, Details) ->
    InfoFun(Details).

add_lineno(Frame, LineNo) when is_integer(LineNo) ->
    Frame ++ [{<<"lineno">>, LineNo}];
add_lineno(Frame, _) ->
    Frame.
