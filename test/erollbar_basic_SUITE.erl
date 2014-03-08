-module(erollbar_basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1
	 ,end_per_suite/1]).

-export([default_settings/1
        ]).

-export([server/2]).

all() ->
    [default_settings
    ].

init_per_suite(Config) ->
    ok = application:start(sasl),
    ok = application:start(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Config.

end_per_suite(Config) ->
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
    receive
        Data ->
            Body = parse_http(Data),
            BodyParsed = jsx:decode(Body),
            AccessCode = proplists:get_value(<<"access_token">>, BodyParsed),
            DataPart = proplists:get_value(<<"data">>, BodyParsed),
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
            ServerPart = proplists:get_value(<<"server">>, BodyParsed),
            {ok, Hostname} = inet:gethostname(),
            Hostname = binary_to_list(proplists:get_value(<<"host">>, ServerPart))
    after 1000 ->
            throw(test_timeout)
    end,
    Config.

% Internal
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
