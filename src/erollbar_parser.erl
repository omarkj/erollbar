-module(erollbar_parser).
-include("erollbar.hrl").
-export([initial_call/1
        ,parse/2]).

initial_call({_, _, {_, _, [Report, _]}}) ->
    {Module, _, _} = proplists:get_value(initial_call, Report),
    Module.

parse({_, _, {_, _, [Report, _]}}, #details{platform=Platform}) ->
    ErrorInfo = proplists:get_value(error_info, Report, []),
    Item = [{<<"trace">>, create_trace(ErrorInfo)},
            {<<"level">>, <<"error">>},
            {<<"timestamp">>, unix_timestamp()},
            {<<"platform">>, Platform},
            {<<"language">>, <<"erlang">>}],
    {ok, Item}.

% Internal
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

add_lineno(Frame, LineNo) when is_integer(LineNo) ->
    Frame ++ [{<<"lineno">>, LineNo}];
add_lineno(Frame, _) ->
    Frame.

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    erollbar_utils:to_binary(Mega*1000000 + Secs).
