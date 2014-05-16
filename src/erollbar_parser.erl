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
create_trace({_Error, ExceptionExit, Frames}) when is_atom(ExceptionExit) ->
    [{<<"frames">>, create_frames(Frames, [])},
     {<<"exception">>, [{<<"class">>, erollbar_utils:to_binary(ExceptionExit)}]}];
create_trace({_Error, {ExceptionExit, Frames}, _}) ->
    [{<<"frames">>, create_frames(Frames, [])},
     {<<"exception">>, [{<<"class">>, erollbar_utils:to_binary(ExceptionExit)}]}].

create_frames([], Retval) ->
    Retval;
create_frames([{Module, Fun, ArgsOrArity, Info}|Rest], Retval) ->
    Filename = iolist_to_binary([erollbar_utils:to_binary(Module), ".erl"]),
    Frame = [{<<"filename">>, erollbar_utils:to_binary(Filename)}],
    Frame1 = add_method_args(Fun, ArgsOrArity, Frame),
    Frame2 = add_lineno(proplists:get_value(line, Info), Frame1),
    create_frames(Rest, Retval ++ [Frame2]).

add_method_args(Fun, Arity, Frame) when is_integer(Arity) ->
    Frame ++ [{<<"method">>, iolist_to_binary([erollbar_utils:to_binary(Fun),
                                               <<"/">>, erollbar_utils:to_binary(Arity)])}];
add_method_args(Fun, Args, Frame) when is_list(Args) ->
    Frame ++ [{<<"method">>, erollbar_utils:to_binary(Fun)},
              {<<"args">>, [erollbar_utils:to_binary(Arg) || Arg <- Args]}].

add_lineno(LineNo, Frame) when is_integer(LineNo) ->
    Frame ++ [{<<"lineno">>, LineNo}];
add_lineno(_, Frame) ->
    Frame.

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    erollbar_utils:to_binary(Mega*1000000 + Secs).
