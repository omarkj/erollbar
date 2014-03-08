-module(erollbar).
-type access_token() :: binary().
-type ms() :: non_neg_integer().
-type info_fun() :: fun(([term()]) -> any()).
-type opt() :: {modules, [module()]}|
               {applications, [atom()]}|
               {environment, binary()}|
               {platform, binary()}|
               {batch_max, pos_integer()}|
               {time_max, ms()}|
               {endpoint, binary()}|
               {info_fun, info_fun()}|
               {host, binary()}|
               {root, binary()}|
               {branch, binary()}|
               {sha, binary()}.
-type opts() :: [opt()]|[].
-define(ENDPOINT, <<"https://api.rollbar.com/api/1">>).
-export_type([access_token/0
              ,opt/0
              ,opts/0
              ,info_fun/0
              ,ms/0]).
-export([start/1
         ,start/2]).

-spec start(access_token()) -> ok.
start(AccessToken) ->
    start(AccessToken, []).

-spec start(access_token(), opts()) -> ok.
start(AccessToken, Opts) ->
    Opts1 = set_defaults([{environment, <<"prod">>}
                          ,{platform, <<"beam">>}
                          ,{batch_max, 10}
                          ,{endpoint, ?ENDPOINT}
                          ,{info_fun, fun info/2}
                          ,{host, hostname()}
                         ], Opts),
    Opts2 = validate_opts(Opts1, []),
    ok = error_logger:add_report_handler(erollbar_handler, [AccessToken, Opts2]).

%% Internal
set_defaults([], Opts) ->
    Opts;
set_defaults([{Key, _}=Pair|Rest], Opts) ->
    case lists:keymember(Key, 1, Opts) of
        true ->
            set_defaults(Rest, Opts);
        false ->
            set_defaults(Rest, Opts++[Pair])
    end.

validate_opts([], Retval) ->
    Retval;
validate_opts([{modules, ModuleList}|Rest], Retval) ->
    validate_opts(Rest, Retval++[{modules, lists:usort(ModuleList)}]);
validate_opts([{applications, ApplicationList}|Rest], Retval) ->
    AppModules = get_application_modules(ApplicationList, []),
    CurrentModules =
        case proplists:get_value(modules, Rest) of
            undefined ->
                proplists:get_value(modules, Retval, []);
            Modules when is_list(Modules) ->
                Modules;
            _ ->
                throw({invalid_config, modules})
        end,
    Rest1 = lists:keydelete(modules, 1, Rest),
    Retval1 = lists:keydelete(modules, 1, Retval),
    validate_opts(Rest1, Retval1++[{modules, lists:usort(AppModules ++ CurrentModules)},
                                   {application, ApplicationList}]);
validate_opts([{Key, _}=Pair|Rest], Retval) ->
    case lists:member(Key, [environment, batch_max, host, endpoint, root, branch,
                            sha, platform, info_fun]) of
        true ->
            validate_opts(Rest, Retval++[Pair]);
        false ->
            throw({invalid_config, Key})
    end.

get_application_modules([], Retval) ->
    Retval;
get_application_modules([App|Rest], Retval) ->
    {ok, Modules} = application:get_key(App, modules),
    get_application_modules(Rest, Retval ++ Modules).

info(undefined, Details) ->
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
