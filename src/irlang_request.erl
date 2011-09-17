%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc The top level supervisor for admin
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_request).
-export([nick/1, user/2, pong/1, join/1]).
-include("irlang.hrl").

nick(Name) -> io_lib:format("NICK ~s\r\n", [Name]).
user(Name, RealName) -> io_lib:format("USER ~s 0 * :~s\r\n", [Name, RealName]).
pong(Message) -> io_lib:format("PONG ~s\r\n", [Message]).
join(Channel) -> io_lib:format("JOIN ~s\r\n", [Channel]).

