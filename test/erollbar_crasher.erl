-module(erollbar_crasher).

-behaviour(gen_server).

%% API
-export([start/0
        ,crash_on_ets/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

% API
start() ->
    gen_server:start(?MODULE, [], []).

crash_on_ets(Pid) ->
    gen_server:cast(Pid, crash_on_ets).

% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(crash_on_ets, State) ->
    ets:insert(heh, bye),
    {reply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions
