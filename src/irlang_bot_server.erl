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
    , init_listen/1 
  ]).

%% Supervisor callbacks
-export([
    init/1, handle_call/3,
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

init(IrcServer = #irc_server{ }) ->
  Pid = proc_lib:spawn_link(?MODULE, init_listen, [IrcServer]),
  {ok, #bot_server_state{loop_pid=Pid} }.

handle_cast(Msg={send, CmdList}, State=#bot_server_state{loop_pid=Pid}) ->
  Pid ! Msg,
  {noreply, State}.

%%
% Synchronous call
%%
handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_info(_Msg, State) -> {noreply, State} .
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================

join(Join = #join{} ) -> 
  {ok, Res} = gen_fsm:sync_send_event(irlang_bot_fsm, {join, Join } ),
  gen_server:cast(?MODULE, {send, Res}).
  
send_cmd(ListCommand, Socket) -> 
  lists:foreach( fun(Cmd) -> ok = ssl:send(Socket, Cmd) end , ListCommand).

%% ===================================================================
%% Listen loop
%% ===================================================================

loop_listen(State = #bot_server_loop{socket=Socket}) -> 
  ssl:setopts(Socket,[{active,true}]),
  receive
    {tcp, Socket, Data} -> 
      {ok, CmdList} = gen_fsm:sync_send_event(irlang_bot_fsm, irlang_request:request_to_event(Data) ),
      send_cmd(CmdList, Socket);
    {send, CmdList} -> 
      send_cmd(CmdList, Socket)
  end,
  loop_listen(State).

init_listen(#irc_server{address=Address, port=Port}) -> 
  Params = [ {active, false} ],
  {ok, Socket} = ssl:connect(Address, Port,  Params, 2000),
  ok = ssl:ssl_accept(Socket),
  loop_listen(#bot_server_loop{socket=Socket} ).

