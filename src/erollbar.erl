-module(erollbar).

-type access_token() :: binary().
-type ms() :: non_neg_integer().
-type info_fun() :: fun(([term()]) -> any()).
-type filter() :: fun((module()) -> ok | drop).
-type opt() :: {environment, binary()}|
               {platform, binary()}|
               {batch_max, pos_integer()}|
               {time_max, ms()}|
               {endpoint, binary()}|
               {info_fun, info_fun()}|
               {host, binary()}|
               {root, binary()}|
               {branch, binary()}|
               {sha, binary()}|
               {filter, filter()}|
               {http_timeout, non_neg_integer()}|
               send_args.
-type opts() :: [opt()].
-define(HTTP_TIMEOUT, 5000).
-define(ENDPOINT, <<"https://api.rollbar.com/api/1">>).
-define(HANDLER_NAME, erollbar_handler).
-export_type([access_token/0
             ,opt/0
             ,opts/0
             ,info_fun/0
             ,filter/0
             ,ms/0]).
-export([start/1
        ,start/2
        ,stop/0]).

-spec start(access_token()) -> ok.
start(AccessToken) ->
    start(AccessToken, []).

-spec start(access_token(), opts()) -> ok.
start(AccessToken, Opts) ->
    Opts1 = set_defaults([{environment, <<"prod">>}
                         ,{platform, <<"beam">>}
                         ,{batch_max, 10}
                         ,{endpoint, ?ENDPOINT}
                         ,{info_fun, fun info/1}
                         ,{host, hostname()}
                         ,{http_timeout, ?HTTP_TIMEOUT}
                         ], Opts),
    Opts2 = validate_opts(Opts1, []),
    ok = error_logger:add_report_handler(?HANDLER_NAME, [AccessToken, Opts2]).

-spec stop() -> ok | term() | {'EXIT', term()}.
stop() ->
    error_logger:delete_report_handler(?HANDLER_NAME).

%% Internal
set_defaults([], Opts) ->
    Opts;
set_defaults([{Key, _}=Pair|Rest], Opts) ->
    case lists:keymember(Key, 1, Opts) of
        true ->
            set_defaults(Rest, Opts);
        false ->
            set_defaults(Rest, [Pair | Opts])
    end.

validate_opts([], Retval) ->
    Retval;
validate_opts([{Key, _}=Pair|Rest], Retval) ->
    case lists:member(Key, [environment, batch_max, host, endpoint, root, branch,
                            sha, platform, info_fun, time_max, filter, http_timeout]) of
        true ->
            validate_opts(Rest, [Pair | Retval]);
        false ->
            throw({invalid_config, Key})
    end;
validate_opts([Opt|Rest], Retval) ->
    case lists:member(Opt, [send_args]) of
        true ->
            validate_opts(Rest, [Opt | Retval]);
        false ->
            throw({invalid_config, Opt})
    end.

info(Details) ->
    {FmtStr, FmtList} = lists:foldl(
                          fun({K, V}, {undefined, FmtLst}) ->
                                  {"~p=~p", FmtLst ++ [K, V]};
                             ({K, V}, {FmtStr, FmtLst}) ->
                                  {FmtStr ++ " ~p=~p", FmtLst ++ [K, V]}
                          end,
                          {undefined, []}, Details),
    error_logger:info_msg(FmtStr, FmtList).

hostname() ->
    {ok, Hostname} = inet:gethostname(),
    list_to_binary(Hostname).
