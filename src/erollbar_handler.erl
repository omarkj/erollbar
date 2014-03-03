-module(erollbar_handler).
-behaviour(gen_event).

-record(state, {access_token :: erollbar:access_token(),
                modules = undefined :: undefined|[module()],
                items = [] :: [term()]|[],
                platform :: binary(),
                environment :: binary(),
                batch_max :: pos_integer(),
                time_max :: erollbar:ms(),
                time_ref :: reference(),
                endpoint :: string(),
                info_fun :: erollbar:info_fun()
               }).

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
                endpoint=binary_to_list(proplists:get_value(endpoint, Opts))
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
    State1 = send_batch(State),
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
    info(InfoFun, [{at, terminate}
                   ,{reason, Reason}
                   ,{dropped, length(Items)}]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal
maybe_send_batch(Item, #state{batch_max=BatchMax,
                              items=Items}=State) when length(Items) + 1 >= BatchMax ->
    send_batch(State#state{items=Items++[Item]});
maybe_send_batch(Item, #state{items=Items}=State) ->
    State#state{items=Items++[Item]}.

send_batch(#state{items=Items,
                  access_token=AccessToken,
                  environment=Environment,
                  endpoint=Endpoint,
                  info_fun=InfoFun}=State) ->
    send_items(Items, InfoFun, Endpoint, AccessToken, Environment),
    State#state{items=[]}.

send_items([], _, _, _, _) ->
    ok;
send_items([Item|Items], InfoFun, Endpoint, AccessToken, Environment) ->
    Message = create_message(AccessToken, Environment, Item),
    case httpc:request(post, {Endpoint ++ "/item/", [], "application/json", Message}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Code, _}, _Headers, Body}} ->
            info(InfoFun, [{mod, erollbar_handler},
                           {at, send_items},
                           {code, Code},
                           {body, Body}]);
        {error, Reason} ->
            info(InfoFun, [{mod, erollbar_handler},
                           {at, send_items},
                           {reason, Reason}])
    end,
    send_items(Items, InfoFun, Endpoint, AccessToken, Environment).

create_message(AccessToken, Environment, Item) ->
    jsx:encode([{<<"access_token">>, AccessToken},
                {<<"data">>, [{<<"environment">>, Environment},
                              {<<"body">>, Item}]}]).

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
            {<<"platform">>, Platform},
            {<<"language">>, <<"erlang">>}],
    {ok, Item}.

create_trace({_Error, Reason, Frames}) ->
    [{<<"frames">>, create_frames(Frames, [])},
     {<<"exception">>,
      [{<<"class">>, Reason}]}].

create_frames([], Retval) ->
    Retval;
create_frames([{Module, Fun, Arity, Info}|Rest], Retval) ->
    Filename = case Module:module_info(compile) of
                   undefined ->
                       Module;
                   CompileInfo ->
                       proplists:get_value(source, CompileInfo)
               end,
    Frame = [{<<"filename">>, list_to_binary(Filename)},
             {<<"lineno">>, proplists:get_value(line, Info)},
             {<<"method">>, iolist_to_binary([atom_to_list(Fun), <<"/">>, integer_to_list(Arity)])}],
    create_frames(Rest, Retval ++ [Frame]).

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    list_to_binary(integer_to_list(Mega*1000000 + Secs)).

info(InfoFun, Details) ->
    InfoFun(Details).
