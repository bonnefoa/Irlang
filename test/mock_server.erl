-module(mock_server).

-export([ start_link/1, start/1 ]).
-export([
    init/1, handle_call/3,
    accept_loop/1,
    handle_cast/2, handle_info/2,
    terminate/2, code_change/3
  ]).
-behaviour(gen_server).

-include("common_test.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
start_link(State) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

start(State) ->
  gen_server:start({local, ?MODULE}, ?MODULE, State, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(State = #server_state{port=Port} ) ->
  Params = [ 
    {reuseaddr, true} 
    , {active, false} 
    , {certfile,"/home/sora/git_repos/irlang/priv/certificate.pem"}
    , {keyfile, "/home/sora/git_repos/irlang/priv/key.pem"}
  ],
  case ssl:listen(Port, Params) of
    {ok, LSocket} ->
      NewState = State#server_state{lsocket = LSocket},
      {ok, accept(NewState)};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
  accept(State),
  {noreply, State}.

accept_loop({Server, LSocket, {M, F}}) ->
  {ok, Socket} = ssl:transport_accept(LSocket),
  ok = ssl:ssl_accept(Socket),
  gen_server:cast(Server, {accepted, self()}),
  M:F({Socket})
  .

accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
  proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
  State.

%% ===================================================================
%% Change loop function on next call
%% ===================================================================
handle_call({change_loop, Loop}, _From, State=#server_state{}) ->
  NewState = State#server_state{loop=Loop},
  accept(NewState),
  {reply, ok, NewState};
handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info(_Msg, State) -> {noreply, State} .
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

