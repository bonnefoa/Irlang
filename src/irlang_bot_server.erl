%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc The irc bot server
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_client).
-include("irlang.hrl").

-behaviour(gen_server).

%% API
-export([start/1, start_link/1]).

%% Supervisor callbacks
-export([init/1, handle_event/3, 
    handle_sync_event/4, handle_info/3,
    terminate/3, code_change/4]). 
%% states
-export([ 
    idle/2, idle/3, 
    connected/2, connected/3, 
    ping/2, ping/3 
  ]). 

%% ===================================================================
%% API functions
%% ===================================================================
start(Args) ->
  gen_fsm:start(?MODULE, Args, []).

start_link(Args) ->
  gen_fsm:start_link(?MODULE, Args, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(#irc_server{ port=Port, address=Address }) ->
  Params = [ {active, true} ],
  {ok, Socket} = ssl:connect(Address, Port,  Params, 2000),
  ok = ssl:ssl_accept(Socket),
  error_logger:warning_msg("INiit"),
  {ok, idle, #state{ socket=Socket }}.

idle(_Event, State=#state{socket=Socket}) -> 
  receive
    #join{ channel=Channel, nick=Nick, real_name=RealName } ->
      ok = ssl:send(Socket, irlang_request:nick(Nick)),
      ok = ssl:send(Socket, irlang_request:user(Nick, RealName)),
      ok = ssl:send(Socket, irlang_request:join(Channel)),
      {next_state, connected, State};
    Msg -> 
      io:format("received ~p", [ Msg ]),
      {next_state, idle, State}
  end .

idle({join, Join=#join{ channel=Channel, nick=Nick, real_name=RealName }, _From, State=#state{socket=Socket}) -> 
    ok = ssl:send(Socket, irlang_request:nick(Nick)),
    ok = ssl:send(Socket, irlang_request:user(Nick, RealName)),
    ok = ssl:send(Socket, irlang_request:join(Channel)),
    {reply, "Connected", connected, State};

idle(_Event, _From, State=#state{socket=Socket}) -> 
  receive
    #join{ channel=Channel, nick=Nick, real_name=RealName } ->
      ok = ssl:send(Socket, irlang_request:nick(Nick)),
      ok = ssl:send(Socket, irlang_request:user(Nick, RealName)),
      ok = ssl:send(Socket, irlang_request:join(Channel)),
      {reply, "Connected", connected, State};
    Msg -> 
      io:format("received ~p", [ Msg ]),
      {reply, "Idiot", idle, State}
  end .

connected(_Event, State) -> 
  receive
    "PING " ++ Msg -> 
      {next_state, ping, State#state{message=Msg} };
    Msg -> 
      io:format("received ~p", [ Msg ]),
      {next_state, connected, State}
  end .

connected(Event, _From, State) -> 
  unexpected(Event, idle),
  {next_state, connected, State} .

ping(_Event, State = #state{socket=Socket, message=Msg}) -> 
  io:format("Ping received", Msg),
  ok = ssl:send(Socket, irlang_request:pong(Msg)),
  {next_state, idle, State} .

ping(Event, _From, State) -> 
  unexpected(Event, ping),
  {next_state, idle, State} .

handle_event(Event, StateName, State) ->
  unexpected(Event, StateName),
  {next_state, idle, State}.

handle_sync_event(Event, _From, StateName, State) ->
  unexpected(Event, StateName),
  {next_state, idle, State}.

handle_info(Info, StateName, State) ->
  unexpected(Info, StateName),
  {idle, idle, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
  error_logger:warning_msg(
    io_lib:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State])
  ).


