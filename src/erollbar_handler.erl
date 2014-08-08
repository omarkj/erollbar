-module(erollbar_handler).
-behaviour(gen_event).

-record(state, {access_token :: erollbar:access_token(),
                items = [] :: [term()]|[],
                requests = [] :: [{reference(), non_neg_integer()}],
                batch_max :: pos_integer()|undefined,
                time_max :: erollbar:ms()|undefined,
                time_ref :: reference()|undefined,
                endpoint :: binary(),
                filter :: erollbar:filter(),
                http_timeout :: non_neg_integer(),
                details :: term()
               }).

-define(MAX_CONN, 10).

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
    ok = hackney_pool:start_pool(erollbar, [{max_connections, ?MAX_CONN}]),
    {ok, #state{access_token=AccessToken,
                batch_max=proplists:get_value(batch_max, Opts),
                time_max=TimeMax,
                time_ref=TimeRef,
                endpoint=proplists:get_value(endpoint, Opts),
                filter=proplists:get_value(filter, Opts),
                http_timeout=proplists:get_value(http_timeout, Opts),
                details=erollbar_parser:prime(Opts)
               }}.

handle_event({error_report, _, {_, crash_report, _}} = Report,
             #state{details=Details,
                    filter=Filter}=State) ->
    InitialCall = erollbar_parser:initial_call(Report),
    case filter(Filter, InitialCall) of
        ok ->
            try erollbar_parser:parse_report(Report, Details) of
                {ok, Item} ->
                    State1 = maybe_send_batch(Item, State),
                    {ok, State1}
            catch
                _:Reason ->
                    info([{mod, erollbar_handler},
                          {at, handle_event},
                          {reason, Reason},
                          {body, Report}])
            end;
        drop ->
            {ok, State}
    end;
handle_event({error, _, {_, Format, Data}} = Error, #state{details=Details, filter=Filter}=State) ->
    case filter(Filter, [error, Format, Data]) of
        ok ->
            try erollbar_parser:parse_message(error, Format, Data, Details) of
                {ok, Item} ->
                    State1 = maybe_send_batch(Item, State),
                    {ok, State1}
            catch
                _:Reason ->
                    info([{mod, erollbar_handler},
                          {at, handle_event},
                          {reason, Reason},
                          {body, Error}])
            end;
        drop ->
            {ok, State}
    end;
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, undefined, State}.

handle_info({hackney_response, RequestRef, Response}, #state{requests=Requests}=State) ->
    case lists:keyfind(RequestRef, 1, Requests) of
        false ->
            {ok, State};
        {RequestRef, ItemsCount} = Element ->
            case handle_response(Response) of
                ok -> ok;
                {error, Reason} ->
                    info([{mod, erollbar_handler},
                          {at, handle_info},
                          {dropped, ItemsCount},
                          {reason, Reason}])
            end,
            {ok, State#state{requests=lists:delete(Element, Requests)}}
    end;
handle_info(time_max_reached, #state{time_max=TimeMax}=State) ->
    State1 = send_items(State),
    TimeRef = erlang:send_after(TimeMax, self(), time_max_reached),
    {ok, State1#state{time_ref=TimeRef}};
handle_info(_Info, State) ->
    {ok, State}.

terminate(Reason, #state{time_ref=TimeRef,
                         items=Items}) ->
    if is_reference(TimeRef) -> erlang:cancel_timer(TimeRef);
       true -> ok
    end,
    catch info([{at, terminate}
               ,{reason, Reason}
               ,{dropped, length(Items)}]),
    hackney_pool:stop_pool(erollbar),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal

%% Only handle the status, as well as errors. The reference to the request is removed
%% from the list of ongoing requests as soon as the status is recieved, so all other
%% parts of the response are safely ignored.
handle_response({status, 200, _Reason}) ->
    ok;
handle_response({status, _Statue, Reason}) ->
    {error, Reason};
handle_response({error, Reason}) ->
    {error, Reason}.

maybe_send_batch(Item, #state{batch_max=BatchMax,
                              items=Items}=State) when length(Items) + 1 >= BatchMax ->
    send_items(State#state{items=[Item | Items]});
maybe_send_batch(Item, #state{items=Items}=State) ->
    State#state{items=[Item | Items]}.

send_items(#state{items=[]}=State) ->
    State;
send_items(#state{requests=Requests, items=Items}=State) when length(Requests) >= ?MAX_CONN ->
    %% All connections are in use. We need to drop this payload and log
    info([{mod, erollbar_handler},
          {at, send_items},
          {dropped, length(Items)},
          {reason, requests_exhausted}]),
    State#state{items = []};
send_items(#state{items=Items, requests=Requests, endpoint=Endpoint,
                  access_token=AccessToken, details=Details,
                  http_timeout=HttpTimeout}=State) ->
    Message = erollbar_parser:encode_message(AccessToken, Items, Details),
    case request(Endpoint, Message, HttpTimeout) of
        {ok, RequestRef} ->
            State#state{items = [], requests = [{RequestRef, length(Items)} | Requests]};
        {error, Reason} ->
            % Unknown error occured. Drop data and log
            info([{mod, erollbar_handler},
                  {at, send_items},
                  {dropped, length(Items)},
                  {reason, Reason}]),
            State#state{items = []}
    end.

filter(undefined, _) ->
    ok;
filter(Filter, InitialCall) ->
    Filter(InitialCall).

info(Details) ->
    {FmtStr, FmtList} = lists:foldl(
                          fun({K, V}, {undefined, FmtLst}) ->
                                  {"~p=~p", FmtLst ++ [K, V]};
                             ({K, V}, {FmtStr, FmtLst}) ->
                                  {FmtStr ++ " ~p=~p", FmtLst ++ [K, V]}
                          end,
                          {undefined, []}, Details),
    error_logger:info_msg(FmtStr, FmtList).

-spec request(binary(), binary(), integer()) -> {ok, reference()} |
                                                {error, term()}.
request(Endpoint, Message, Timeout) ->
    %% Async request to not block the event manager
    Resource = <<Endpoint/binary, "/items/">>,
    hackney:request(post, Resource, [{<<"content-type">>, <<"application/json">>}],
                    Message, [{timeout, Timeout}, async]).
