
-module(irlang_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_ARGS(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Server) ->
  RestartStrategy    = one_for_one,
  MaxRestarts        = 1,
  MaxTimeBetRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
  ChildSpecs = [ 
    ?CHILD_ARGS(irlang_bot_server, worker, [Server]) 
    , ?CHILD(irlang_bot_fsm, worker) 
    
  ],
  {ok,{SupFlags, ChildSpecs}}.

