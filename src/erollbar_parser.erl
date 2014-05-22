-module(erollbar_parser).
-include("erollbar.hrl").

-export([initial_call/1
        ,parse/2]).

initial_call({_, _, {_, _, [Report, _]}}) ->
    {Module, _, _} = proplists:get_value(initial_call, Report),
    Module.

parse({_, _, {_, _, [Report, _]}}, #details{platform=Platform}=Details) ->
    ErrorInfo = proplists:get_value(error_info, Report, []),
    Item = [{<<"trace">>, create_trace(ErrorInfo, Details)},
            {<<"level">>, <<"error">>},
            {<<"timestamp">>, unix_timestamp()},
            {<<"platform">>, Platform},
            {<<"language">>, <<"erlang">>}],
    {ok, Item}.

% Internal
create_trace({_Error, ExceptionExit, Frames}, Details) when is_atom(ExceptionExit) ->
    [{<<"frames">>, create_frames(Frames, Details, [])},
     {<<"exception">>, [{<<"class">>, erollbar_utils:to_binary(ExceptionExit)}]}];
create_trace({_Error, {ExceptionExit, Frames}, _}, Details) ->
    [{<<"frames">>, create_frames(Frames, Details, [])},
     {<<"exception">>, [{<<"class">>, erollbar_utils:to_binary(ExceptionExit)}]}].

create_frames([], _, Retval) ->
    Retval;
create_frames([{Module, Fun, ArgsOrArity, Info}|Rest], #details{send_args=SendArgs}=Details,
              Retval) ->
    Filename = iolist_to_binary([erollbar_utils:to_binary(Module), ".erl"]),
    Frame = [{<<"filename">>, erollbar_utils:to_binary(Filename)}],
    Frame1 = add_method_args(Fun, ArgsOrArity, Frame, SendArgs),
    Frame2 = add_lineno(proplists:get_value(line, Info), Frame1),
    create_frames(Rest, Details, Retval ++ [Frame2]).

add_method_args(Fun, Arity, Frame, _) when is_integer(Arity) ->
    Frame ++ [{<<"method">>, iolist_to_binary([erollbar_utils:to_binary(Fun),
                                               <<"/">>, erollbar_utils:to_binary(Arity)])}];
add_method_args(Fun, Args, Frame, SendArgs) when is_list(Args) ->
    if SendArgs ->
            Frame ++ [{<<"method">>, erollbar_utils:to_binary(Fun)},
                      {<<"args">>, [erollbar_utils:to_binary(Arg) || Arg <- Args]}];
       true ->
            add_method_args(Fun, length(Args), Frame, SendArgs)
    end.

add_lineno(LineNo, Frame) when is_integer(LineNo) ->
    Frame ++ [{<<"lineno">>, LineNo}];
add_lineno(_, Frame) ->
    Frame.

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    erollbar_utils:to_binary(Mega*1000000 + Secs).
