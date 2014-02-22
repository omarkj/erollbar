-module(erollbar_basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1
	 ,end_per_suite/1]).

-export([default_settings/1
        ]).


all() ->
    [default_settings].

init_per_suite(Config) ->
    ok = application:start(sasl),
    Config.

end_per_suite(Config) ->
    ok = application:stop(sasl),
    Config.

%% Tests
default_settings(Config) ->
    % Get a crash report
    erollbar:start(lol),
    proc_lib:spawn(fun() -> 1/0 end),
    timer:sleep(10),
    Config.
