-module(erollbar_message).

%% @doc This module is used to create erollbar records that can be
%% safely converted into records that can be sent to Rollbar.com

%% Create records
-export([trace/1
        ,frame/1
        ,message/1
        ]).

%% Fill them
-export([message/2
        ,description/2
        ,lineno/2
        ,colno/2
        ,method/2
        ,code/2
        ,args/2
        ,metadata/3
        ,frames/2
        ,level/2
        ]).

%% Getters
-export([type/1
        ,get_proplist/1]).

-type level() :: critical|error|warning|info|debug.
-type erollbar_string() :: iolist()|atom()|binary().
-type lineno() :: non_neg_integer().
-type colno() :: non_neg_integer().
-type method() :: erollbar_string().
-type filename() :: erollbar_string().
-type code() :: erollbar_string().
-type args() :: [term()].
-type class() :: erollbar_string().
-type message() :: erollbar_string().
-type description() :: erollbar_string().
-type metadata() :: [{erollbar_string(), term()}].

-record(frame, {
          filename :: filename(),
          lineno :: lineno(),
          colno :: colno(),
          method :: method(),
          code :: code(),
          args = [] :: args()
         }).

-record(trace, {
          timestamp = os:timestamp() :: erlang:timestamp(),
          level :: level(),
          class :: class(),
          message :: message(),
          description :: description(),
          frames = [] :: [#frame{}]
         }).

-record(message, {
          timestamp = os:timestamp() :: erlang:timestamp(),
          level :: level(),
          body :: message(),
          metadata = [] :: metadata()
         }).

-opaque trace() :: #trace{}.
-opaque msg() :: #message{}.
-opaque frame() :: #frame{}.
-type erollbar_msg() :: trace()|msg()|frame().
-type trace_list() :: [{timestamp, erlang:timestamp()}|
                       {level, level()}|
                       {class, class()}|
                       {message, message()}|
                       {description, description()}|
                       {frames, [frame()]}].
-type msg_list() :: [{timestamp, erlang:timestamp()}|
                     {level, level()}|
                     {class, class()}|
                     {body, message()}|
                     {metadata, metadata()}].
-type frame_list() :: [{filename, filename()}|
                       {lineno, lineno()}|
                       {colno, colno()}|
                       {method, method()}|
                       {code, code()}|
                       {args, args()}].

%% Message types
-export_type([trace/0, msg/0, frame/0,
              trace_list/0, msg_list/0, frame_list/0,
              erollbar_msg/0]).

%% Other types
-export_type([level/0, erollbar_string/0, lineno/0, colno/0,
              method/0, filename/0, code/0, args/0, class/0,
              message/0, description/0, metadata/0]).

%% @doc Create a new trace message.
-spec trace(Class) -> Trace when
      Class :: class(),
      Trace :: trace().
trace(Class) ->
    #trace{class=Class}.

%% @doc Create a new frame message.
-spec frame(Filename) -> Frame when
      Filename :: filename(),
      Frame :: frame().
frame(Filename) ->
    #frame{filename=Filename}.

%% @doc Create a new message message
-spec message(Body) -> Message when
      Body :: message(),
      Message :: msg().
message(Body) ->
    #message{body=Body}.

%% @doc Add a Message to a Trace message
-spec message(Message, ErollbarMessage) -> ErollbarMessage when
      Message :: message(),
      ErollbarMessage :: trace().
message(Message, #trace{}=TraceMessage) ->
    TraceMessage#trace{message=Message}.

%% @doc Add a Description to a Trace message
-spec description(Description, ErollbarMessage) -> ErollbarMessage when
      Description :: description(),
      ErollbarMessage :: trace().
description(Description, #trace{}=TraceMessage) ->
    TraceMessage#trace{description=Description}.

%% @doc Add a Line Number to a Frame message
-spec lineno(LineNo, ErollbarMessage) -> ErollbarMessage when
      LineNo :: lineno(),
      ErollbarMessage :: frame().
lineno(LineNo, #frame{}=Frame) ->
    Frame#frame{lineno=LineNo}.

%% @doc Add a Column Number to a Frame message
-spec colno(ColNo, ErollbarMessage) -> ErollbarMessage when
      ColNo :: colno(),
      ErollbarMessage :: frame().
colno(ColNo, #frame{}=Frame) ->
    Frame#frame{colno=ColNo}.

%% @doc Add a Method to a Frame message
-spec method(Method, ErollbarMessage) -> ErollbarMessage when
      Method :: method(),
      ErollbarMessage :: frame().
method(Method, #frame{}=Frame) ->
    Frame#frame{method=Method}.

%% @doc Add a Code block to a Frame message
-spec code(Code, ErollbarMessage) -> ErollbarMessage when
      Code :: code(),
      ErollbarMessage :: frame().
code(Code, #frame{}=Frame) ->
    Frame#frame{code=Code}.

%% @doc Add Arguments to a Frame message
-spec args(Args, ErollbarMessage) -> ErollbarMessage when
      Args :: args(),
      ErollbarMessage :: frame().
args(Args, #frame{}=Frame) ->
    Frame#frame{args=Args}.

%% @doc Add metadata to a Message message
-spec metadata(Key, Val, ErollbarMessage) -> ErollbarMessage when
      Key :: erollbar_string(),
      Val :: term(),
      ErollbarMessage :: msg().
metadata(Key, Val, #message{metadata=Metadata}=Message) ->
    Message#message{metadata=[{Key, Val}|Metadata]}.

%% @doc Add Frames to a Trace message
-spec frames(Frames, ErollbarMessage) -> ErollbarMessage when
      Frames :: [frame()],
      ErollbarMessage :: trace().
frames(Frames, #trace{}=TraceMessage) ->
    TraceMessage#trace{frames=Frames}.

%% @doc Add a reporting Level to a Message message or Trace message
-spec level(Level, ErollbarMessage) -> ErollbarMessage when
      Level :: level(),
      ErollbarMessage :: trace()|msg().
level(Level, #message{}=Message) ->
    Message#message{level=Level};
level(Level, #trace{}=TraceMessage) ->
    TraceMessage#trace{level=Level}.

%% @doc Get the type of message
-spec type(ErollbarMessage) -> frame|message|trace when
      ErollbarMessage :: trace()|msg()|frame().
type(#frame{}) -> frame;
type(#message{}) -> message;
type(#trace{}) -> trace. 

%% @doc Get the message as a proplist for further processing
-spec get_proplist(ErollbarMessage) -> ErollbarList when
      ErollbarMessage :: trace()|msg()|frame()|
                         [trace()|msg()|frame()],
      ErollbarList :: trace()|msg()|frame()|
                      [trace_list()|msg_list()|frame_list()].
get_proplist(#frame{filename=Filename,
                    lineno=LineNo,
                    colno=ColNo,
                    method=Method,
                    code=Code,
                    args=Args}) ->
    [{filename, Filename}
    ,{lineno, LineNo}
    ,{colno, ColNo}
    ,{method, Method}
    ,{code, Code}
    ,{args, Args}];
get_proplist(#message{timestamp=Timestamp,
                      level=Level,
                      body=Body,
                      metadata=Metadata}) ->
    [{timestamp, Timestamp}
    ,{level, Level}
    ,{body, Body}
    ,{metadata, Metadata}];
get_proplist(#trace{timestamp=Timestamp,
                    level=Level,
                    class=Class,
                    message=Message,
                    description=Description,
                    frames=Frames}) ->
    [{timestamp, Timestamp}
    ,{level, Level}
    ,{class, Class}
    ,{message, Message}
    ,{description, Description}
    ,{frames, Frames}];
get_proplist(List) when is_list(List) ->
    [get_proplist(Item) || Item <- List].
