-module(irlang_app).

-behaviour(application).

-include("irlang.hrl").

%% API
-export([start/0, shutdown/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, [Port, Address] ) ->
    Server=#irc_server{ port=Port, address=Address },
    irlang_sup:start_link([ Server ]).

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc The starting point for the application
%% @spec start() -> ok | {error, Reason} 
%% @end
%%--------------------------------------------------------------------
start() -> application:start(irlang).

%%--------------------------------------------------------------------
%% @doc Called to shudown the irlang application.
%% @spec shutdown() -> ok
%% @end
%%--------------------------------------------------------------------
shutdown() -> application:stop(irlang).

