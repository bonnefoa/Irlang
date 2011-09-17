%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc The socket manager
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_server).
-export([ start_link/1, start/1 ]).
-export([
    init/1, handle_call/3,
    accept_loop/1,
    handle_cast/2, handle_info/2,
    terminate/2, code_change/3
  ]).
-behaviour(gen_server).
-include("irlang_server.hrl").

start_link(State) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

start(State) ->
  gen_server:start({local, ?MODULE}, ?MODULE, State, []).

%%%-------------------------------------------------------------------
%%% @doc The gen_server init function
%%% @end
%%% Port: int()
%%%-------------------------------------------------------------------
init(State = #server_state{port=Port}) ->
  ssl:start(),
  Params = [ {reuseaddr, true} , {active, false}
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

accept_loop({Server, LSocket, {M, F}, Domain}) ->
  {ok, Socket} = ssl:transport_accept(LSocket),
  ok = ssl:ssl_accept(Socket),
  gen_server:cast(Server, {accepted, self()}),
  M:F({Socket, Domain}).

accept(State = #server_state{lsocket=LSocket, loop = Loop, domain=Domain}) ->
  proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket, Loop, Domain}]),
  State.

%%
% Synchronous call
%%
handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_info(_Msg, State) -> {noreply, State} .
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

