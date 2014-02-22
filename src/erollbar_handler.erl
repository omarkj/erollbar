-module(erollbar_handler).
-behaviour(gen_event).

-record(state, {access_token :: erollbar:access_token(),
                modules = undefined :: undefined|[module()],
                items = [] :: [term()]|[],
                platform :: iolist()
               }).

-export([init/1
         ,handle_event/2
         ,handle_call/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3]).

init([AccessToken, Opts]) ->
    {ok, #state{access_token=AccessToken,
                platform=proplists:get_value(platform, Opts),
                modules=proplists:get_value(modules, Opts)}}.

handle_event({error_report, _, {_, crash_report, _}} = ErrorReport, State) ->
    case maybe_create_item(ErrorReport, State) of
        undefined ->
            {ok, State};
        {ok, Item} ->
            {ok, State#state{items=State#state.items++[Item]}}
    end;
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, undefined, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal
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
    Frame = [{<<"filename">>, Filename},
             {<<"lineno">>, proplists:get_value(line, Info)},
             {<<"method">>, [Fun, <<"/">>, Arity]}],
    create_frames(Rest, Retval ++ [Frame]).

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    integer_to_list(Mega*1000000 + Secs).
