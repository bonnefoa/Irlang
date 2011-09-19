%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc The irc bot server
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_bot_server).
-include("irlang.hrl").

-behaviour(gen_server).

% API
-export([
    start/1, start_link/1
    , join/1 
  ]).

%% Supervisor callbacks
-export([
    init/1, handle_call/3,
    accept_loop/1,
    handle_cast/2, handle_info/2,
    terminate/2, code_change/3
  ]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(#irc_server{ port=Port, address=Address }) ->
  Params = [ {active, false} ],
  {ok, Socket} = ssl:connect(Address, Port,  Params, 2000),
  ok = ssl:ssl_accept(Socket),
  State = #bot_server_state{socket = Socket},
  {ok, accept(State)}.

handle_cast(accepted, State=#bot_server_state{}) ->
  accept(State),
  {noreply, State}.

accept_loop({Server, #bot_server_state{socket=Socket}}) ->
  {ok, Data} = ssl:recv(Socket, 0), 
  gen_server:cast(Server, accepted),
  {ok, RespList} = gen_fsm:sync_send_event(irlang_bot_fsm, irlang_request:request_to_event(Data) ),
  send_cmd(RespList, Socket).

accept(State) ->
  proc_lib:spawn_link(?MODULE, accept_loop, [{self(), State}]),
  State.

%%
% Synchronous call
%%
handle_call({request, ListRequest}, _From, State=#bot_server_state{socket=Socket}) -> 
  send_cmd(ListRequest, Socket),
  {reply, ok, State};
handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info(_Msg, State) -> {noreply, State} .
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

join(Join = #join{} ) -> 
  {ok, Res} = gen_fsm:sync_send_event(irlang_bot_fsm, {join, Join } ),
  gen_server:call(?MODULE, {request, Res}).
  
send_cmd(ListCommand, Socket) -> 
  lists:foreach( fun(Cmd) -> ok = ssl:send(Socket, Cmd) end , ListCommand).

