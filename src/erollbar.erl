-module(erollbar).
-type access_token() :: binary().
-type opt() :: {modules, [module()]}|
               {applications, [atom()]}|
               {environment, iolist()}|
               {platform, iolist()}|
               {batch_max, pos_integer()}|
               {host, iolist()}|
               {endpoint, iolist()}|
               {root, iolist()}|
               {branch, iolist()}|
               {code_version, iolist()}.
-type opts() :: [opt()]|[].
-define(ENDPOINT, <<"https://api.rollbar.com/api/1/">>).
-export_type([access_token/0
              ,opt/0
              ,opts/0]).
-export([start/1
         ,start/2]).

start(AccessToken) ->
    start(AccessToken, []).

start(AccessToken, Opts) ->
    Opts1 = set_defaults([{environment, <<"prod">>}
                          ,{platform, <<"beam">>}
                          ,{host, hostname()}
                          ,{endpoint, ?ENDPOINT}], Opts),
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
                            code_version, platform]) of
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

hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.
