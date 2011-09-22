%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc The irc client
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_bot_fsm).
-include("irlang.hrl").

-behaviour(gen_fsm).

-define(BASE_PV, 3).

%% API
-export([start/0, start_link/0]).

%% Supervisor callbacks
-export([init/1, handle_event/3,
    handle_sync_event/4, handle_info/3,
    terminate/3, code_change/4]).
%% states
-export([
    idle/3,
    joined/3
  ]).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
  gen_fsm:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
  {ok, idle, #bot_server_state{ } }.

%% ===================================================================
%% Idle state
%% ===================================================================

idle({join, #join{ channel=Channel, nick=Nick, real_name=RealName } } , _From, State) ->
  Reply = {ok, [ irlang_request:nick(Nick), irlang_request:user(Nick, RealName), irlang_request:join(Channel) ]},
  {reply, Reply, joined, State#bot_server_state{nick=Nick, channel=Channel, pv=?BASE_PV} };

idle(Event, _From, State) ->
  unexpected_state(Event, idle, State).

%% ===================================================================
%% Joined state
%% ===================================================================

joined(_Event, _From, State = #bot_server_state{channel=Channel, pv=0}) ->
  Reply = {ok, [ irlang_request:message(Channel, "You are ugly and stupid!") ]},
  {stop, he_mad, Reply, State};

joined({disconnect, Reason}, _From, State) ->
  Reply = {ok, [ irlang_request:quit(Reason) ]},
  {reply, Reply, joined, State };

joined({ping, Msg}, _From, State) ->
  Reply = {ok, [ irlang_request:pong(Msg) ]},
  {reply, Reply, joined, State };

joined({priv_msg, #msg{message=Msg} }, _From, State = #bot_server_state{channel=Channel, pv=Pv, nick=Nick} ) ->
  case irlang_request:is_message_addressed_to(Msg, Nick) of
    true ->
      ChannelReply = lists:flatten(io_lib:format("You mad? (~w pv left)", [Pv] )),
      Reply = {ok, [ irlang_request:message(Channel, ChannelReply) ] },
      {reply, Reply, joined, State#bot_server_state{pv=Pv-1} };
    false ->
      {reply, {ok, []}, joined, State }
  end;

joined(Event, _From, State) ->
  unexpected_state(Event, joined, State).

%% ===================================================================
%%
%% ===================================================================

handle_event(Event, StateName, State) ->
  unexpected(Event, StateName),
  {next_state, idle, State}.

handle_sync_event(stop, _From, _StateName, State) ->
  {stop, quit, State};

handle_sync_event(Event, _From, StateName, State) ->
  error_logger:error_msg("Got unexpected event ~p while state is ~p~n", [Event, StateName]),
  {reply, {ko, "Unexpected request"}, StateName, State}.

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

unexpected_state(Event, StateName, _State) ->
  error_logger:error_msg("Got unexpected event ~p while state is ~p~n", [Event, StateName]),
  throw({unexpected_event, Event}).

