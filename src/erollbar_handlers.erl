-module(erollbar_handlers).

%%%%
%% @doc Erollbar handlers are functions that the Erollbar libray calls for every
%% incoming message. They can return `pass`, which causes the library to call the
%% next handler, `ignore` which drops the message or `{ok, erollbar_message:message()}
%% which causes the message to be processed.
%%%

-export([default_handlers/0]).

-export([filter_doubles/1
        ,filter_erollbar/1
        ,crash_report/1
        ,error_msg/1
        ]).

-type handler() :: fun((term()) -> {ok, erollbar_message:erollbar_msg()}|pass|ignore).
-export_type([handler/0]).

%% @doc Erollbar's default handlers
-spec default_handlers() -> [handler()].
default_handlers() ->
    [fun ?MODULE:filter_doubles/1
    ,fun ?MODULE:filter_erollbar/1
    ,fun ?MODULE:crash_report/1
    ,fun ?MODULE:error_msg/1
    ].


%% @doc Filters out messages that are also sent as crash_reports to prevent double
%% reporting.
filter_doubles({error, _, {_, "** Generic server ~p terminating \n** Last message" ++
                               " in was ~p~n** When Server state == ~p~n**" ++
                               " Reason for termination == ~n** ~p~n", _Data}}) ->
    ignore;
filter_doubles(_) ->
    pass.


%% @doc Filters out errors from the erollbar library
filter_erollbar({info_report, _, {_, erollbar_report, _}}) ->
    ignore;
filter_erollbar(_) ->
    pass.

%% @doc Handles Crash Reports
crash_report({error_report, _, {_, crash_report, [Report, _]}}) ->
    case proplists:get_value(error_info, Report, []) of
        [] ->
            pass;
        {_Error, {ExceptionExit, Frames}, _} ->
            TraceMessage = erollbar_message:trace(ExceptionExit),
            ParsedFrames = create_frames(Frames),
            TraceMessage1 = erollbar_message:frames(ParsedFrames, TraceMessage),
            TraceMessage2 = erollbar_message:level(error, TraceMessage1),
            {ok, TraceMessage2};
        {_Error, ExceptionExit, Frames} ->
            TraceMessage = erollbar_message:trace(ExceptionExit),
            ParsedFrames = create_frames(Frames),
            TraceMessage1 = erollbar_message:frames(ParsedFrames, TraceMessage),
            TraceMessage2 = erollbar_message:level(error, TraceMessage1),
            {ok, TraceMessage2}
    end;
crash_report(_) ->
    pass.


%% @doc Handles error messages
error_msg({error, _, {_, Format, Data}}) ->
    Message = erollbar_message:message(io_lib:format(Format, Data)),
    Message1 = erollbar_message:metadata(<<"type">>, <<"error">>, Message),
    Message2 = erollbar_message:level(error, Message1),
    {ok, Message2};
error_msg(_) ->
    pass.

% Internal
create_frames(Frames) ->
    create_frames(Frames, []).

create_frames([], Retval) ->
    lists:reverse(Retval);
create_frames([{Module, Fun, ArgsOrArity, Info}|Rest], Retval) ->
    Frame = erollbar_message:frame(atom_to_list(Module) ++ ".erl"),
    Frame1 = case proplists:get_value(line, Info) of
                 LineNo when is_integer(LineNo) ->
                     erollbar_message:lineno(LineNo, Frame);
                 _ ->
                     Frame
             end,
    Frame2 = erollbar_message:method(Fun, Frame1),
    Frame3 = erollbar_message:args(ArgsOrArity, Frame2),
    create_frames(Rest, [Frame3|Retval]).
