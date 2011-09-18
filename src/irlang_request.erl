%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc Operation for irc
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_request).
-export([
    nick/1, user/2, pong/1, ping/1,
    join/1, private_message/2, quit/1
    , request_to_event/1
  ]).
-include("irlang.hrl").

-define(WRITE_CMD(Str, Args), lists:flatten(io_lib:format(Str, Args))).

nick(Name)                     -> ?WRITE_CMD("NICK ~s\r\n", [Name]).
user(Name, RealName)           -> ?WRITE_CMD("USER ~s 0 * :~s\r\n", [Name, RealName]).
pong(Message)                  -> ?WRITE_CMD("PONG ~s\r\n", [Message]).
ping(Message)                  -> ?WRITE_CMD("PING ~s\r\n", [Message]).
join(Channel)                  -> ?WRITE_CMD("JOIN ~s\r\n", [Channel]).
private_message(User, Message) -> ?WRITE_CMD("PRIVMSG ~s ~s\r\n", [User, Message]).
quit(Reason)                   -> ?WRITE_CMD("QUIT ~s\r\n", [Reason]).

request_to_event(Request) -> 
  case Request of 
    "PING " ++ Msg -> {ping, Msg}
  end.

