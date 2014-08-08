-module(erollbar_basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1
        ,end_per_suite/1
        ,init_per_testcase/2
        ,end_per_testcase/2
        ]).

-export([default_settings/1
        ,max_time/1
        ,bigger_crash/1
        ,crash_with_args/1
        ]).

-export([server/2]).

all() ->
    [default_settings
    ,max_time
    ,bigger_crash
    ,crash_with_args
    ].

init_per_suite(Config) ->
    ok = application:start(sasl),
    {ok, _} = application:ensure_all_started(erollbar),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    ct:pal("stopping"),
    ok = erollbar:stop(),
    ct:pal("stopped"),
    Config.
    
%% Tests
default_settings(Config) ->
    % Get a crash report
    Self = self(),
    Port = create([{ondata, fun(ASocket, Data, _) ->
                                    Self ! Data,
                                    gen_tcp:send(ASocket, "HTTP/1.1 200 OK\r\n\r\n"),
                                    die
                            end}]),
    AccessCode = <<"test_code">>,
    erollbar:start(AccessCode, [{endpoint, list_to_binary("http://127.0.0.1:"++integer_to_list(Port))},
                                {batch_max, 0}]),
    proc_lib:spawn(fun() -> 1/0 end),
    [Data] = get_msg(1),
    Body = parse_http(Data),
    BodyParsed = jsx:decode(Body),
    AccessCode = proplists:get_value(<<"access_token">>, BodyParsed),
    [DataPart] = proplists:get_value(<<"data">>, BodyParsed),
    <<"prod">> = proplists:get_value(<<"environment">>, DataPart),
    BodyPart = proplists:get_value(<<"body">>, DataPart),
    <<"error">> = proplists:get_value(<<"level">>, BodyPart),
    <<"beam">> = proplists:get_value(<<"platform">>, BodyPart),
    <<"erlang">> = proplists:get_value(<<"language">>, BodyPart),
    Trace = proplists:get_value(<<"trace">>, BodyPart),
    [Frame1, _] = proplists:get_value(<<"frames">>, Trace),
    <<"-default_settings/1-fun-1-/0">> = proplists:get_value(<<"method">>, Frame1),
    Filename = proplists:get_value(<<"filename">>, Frame1),
    {_, _} = binary:match(Filename, <<"erollbar_basic_SUITE.erl">>),
    ServerPart = proplists:get_value(<<"server">>, DataPart),
    {ok, Hostname} = inet:gethostname(),
    Hostname = binary_to_list(proplists:get_value(<<"host">>, ServerPart)),
    Config.

max_time(Config) ->
    Self = self(),
    Port = create([{ondata, fun(ASocket, Data, _) ->
                                    Self ! Data,
                                    gen_tcp:send(ASocket, "HTTP/1.1 200 OK\r\n\r\n"),
                                    die
                            end}]),
    AccessCode = <<"test_code">>,
    erollbar:start(AccessCode, [{endpoint, list_to_binary("http://localhost:"++integer_to_list(Port))},
                                {batch_max, 15}, {time_max, 10}]),
    proc_lib:spawn(fun() -> 1/0 end),
    timer:sleep(10),
    [_] = get_msg(1),
    Config.

bigger_crash(Config) ->
    Self = self(),
    Port = create([{ondata, fun(ASocket, Data, _) ->
                                    Self ! Data,
                                    gen_tcp:send(ASocket, "HTTP/1.1 200 OK\r\n\r\n"),
                                    die
                            end}]),
    AccessCode = <<"test_code">>,
    erollbar:start(AccessCode, [{endpoint,
                                 list_to_binary("http://localhost:"++integer_to_list(Port))}
                               ,{batch_max, 0}]),
    {ok, Pid} = erollbar_crasher:start(),
    catch erollbar_crasher:crash_on_ets(Pid),
    timer:sleep(10),
    [Data] = get_msg(1),
    Body = parse_http(Data),
    BodyParsed = jsx:decode(Body),
    AccessCode = proplists:get_value(<<"access_token">>, BodyParsed),
    [DataPart] = proplists:get_value(<<"data">>, BodyParsed),
    <<"prod">> = proplists:get_value(<<"environment">>, DataPart),
    BodyPart = proplists:get_value(<<"body">>, DataPart),
    <<"error">> = proplists:get_value(<<"level">>, BodyPart),
    <<"beam">> = proplists:get_value(<<"platform">>, BodyPart),
    <<"erlang">> = proplists:get_value(<<"language">>, BodyPart),
    Trace = proplists:get_value(<<"trace">>, BodyPart),
    [Frame1, _, _, _] = proplists:get_value(<<"frames">>, Trace),
    <<"insert/2">> = proplists:get_value(<<"method">>, Frame1),
    Filename = proplists:get_value(<<"filename">>, Frame1),
    {_, _} = binary:match(Filename, <<"ets.erl">>),
    undefined = proplists:get_value(<<"args">>, Frame1),
    Config.

crash_with_args(Config) ->
    Self = self(),
    Port = create([{ondata, fun(ASocket, Data, _) ->
                                    Self ! Data,
                                    gen_tcp:send(ASocket, "HTTP/1.1 200 OK\r\n\r\n"),
                                    die
                            end}]),
    AccessCode = <<"test_code">>,
    erollbar:start(AccessCode, [{endpoint,
                                 list_to_binary("http://localhost:"++integer_to_list(Port))}
                               ,{batch_max, 0}, send_args]),
    {ok, Pid} = erollbar_crasher:start(),
    catch erollbar_crasher:crash_on_ets(Pid),
    timer:sleep(10),
    [Data] = get_msg(1),
    Body = parse_http(Data),
    BodyParsed = jsx:decode(Body),
    [DataPart] = proplists:get_value(<<"data">>, BodyParsed),
    BodyPart = proplists:get_value(<<"body">>, DataPart),
    Trace = proplists:get_value(<<"trace">>, BodyPart),
    [Frame1, _, _, _] = proplists:get_value(<<"frames">>, Trace),
    <<"insert">> = proplists:get_value(<<"method">>, Frame1),
    Filename = proplists:get_value(<<"filename">>, Frame1),
    {_, _} = binary:match(Filename, <<"ets.erl">>),
    [<<"heh">>, <<"bye">>] = proplists:get_value(<<"args">>, Frame1),
    Config.

% Internal
get_msg(Count) ->
    get_msg(Count, []).

get_msg(0, Retval) ->
    Retval;
get_msg(N, Retval) ->
    receive
        Data ->
            get_msg(N-1, Retval++[Data])
    after 6000 ->
            throw(test_timeout)
    end.

parse_http(Data) ->
    {ok, Request, Remainder} = erlang:decode_packet(http, Data, []),
    consume_headers([Request], Remainder).

consume_headers(Headers, Data) ->
    case erlang:decode_packet(httph, Data, []) of
        { ok, http_eoh, Rest } ->
            Rest ;
        { ok, Header, Rest } ->
            consume_headers(Headers ++ [Header], Rest)
    end.

create(Opts) ->
    Acceptors = proplists:get_value(acceptors, Opts, 1),
    {ok, LSocket} = gen_tcp:listen(0, [{active, false}, binary]),
    {ok, Port} = inet:port(LSocket),
    _Pids = create_servers(LSocket, Acceptors, Opts, []),
    Port.

create_servers(_, 0, _Opts, Pids) ->
    Pids;
create_servers(LSocket, Acceptors, Opts, Pids) ->
    Pid = spawn(?MODULE, server, [LSocket, Opts]),
    create_servers(LSocket, Acceptors-1, Opts, [Pid|Pids]).

server(LSocket, Opts) ->
    {ok, ASocket} = gen_tcp:accept(LSocket),
    State = proplists:get_value(state, Opts, undefined),
    loop(ASocket, Opts, State),
    case proplists:get_value(restart, Opts, false) of
        true ->
            server(LSocket, Opts);
        false ->
            ok
    end.

loop(ASocket, Opts, State) ->
    inet:setopts(ASocket, [{active, once}]),
    OnLoop = proplists:get_value(ondata, Opts, undefined),
    receive
        {tcp, _Socket, Data} ->
            case OnLoop of
                F when is_function(F) ->
                    case F(ASocket, Data, State) of
                        die ->
                            ok;
                        NewState ->
                            loop(ASocket, Opts, NewState)
                    end;
                _ ->
                    gen_tcp:send(ASocket, "HTTP/1.1 200 OK\r\n\r\n")
            end;
        {tcp_closed, _Socket} ->
            ok
    end.
