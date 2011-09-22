%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc Operation for irc
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_request).
-export([
    nick/1, user/2, pong/1, ping/1,
    join/1, message/3, message/2, quit/1
    , request_to_event/1
    , is_message_addressed_to/2
  ]).
-include("irlang.hrl").

-define(WRITE_CMD(Str, Args), lists:flatten(io_lib:format(Str, Args))).

nick(Name)                     -> ?WRITE_CMD("NICK ~s\r\n", [Name]).
user(Name, RealName)           -> ?WRITE_CMD("USER ~s 0 * :~s\r\n", [Name, RealName]).
pong(Message)                  -> ?WRITE_CMD("PONG ~s\r\n", [Message]).
ping(Message)                  -> ?WRITE_CMD("PING ~s\r\n", [Message]).
join(Channel)                  -> ?WRITE_CMD("JOIN ~s\r\n", [Channel]).
message(Channel, User, Message)-> ?WRITE_CMD("PRIVMSG ~s ~s :~s\r\n", [Channel, User, Message]).
message(Channel, Message)      -> ?WRITE_CMD("PRIVMSG ~s :~s\r\n",    [Channel, Message]).
quit(Reason)                   -> ?WRITE_CMD("QUIT ~s\r\n", [Reason]).


%% ===================================================================
%% @doc Remove ending \r\n in the message
%% @end
%% ===================================================================

remove_end_chars(Str) ->
  string:substr(Str, 1, string:len(Str) - 2 ).

extract_pseudo(From) ->
  string:substr(From, 2, string:chr(From, $!) - 2 ).

is_message_addressed_to(Msg, Nick) ->
  Pref = ":" ++ Nick ++ ":",
  string:str(Msg, Pref) =:= 1 .

request_to_event(Request) ->
  SplittedRequest = string:tokens(Request, " "),
  case SplittedRequest of
    ["PING", MsgRaw] ->
      Msg = remove_end_chars(MsgRaw),
      {ping, Msg};
    [FromRaw | [ "PRIVMSG" | [ Channel | MsgRaw ] ] ] ->
      From = extract_pseudo(FromRaw),
      Msg = remove_end_chars(string:join(MsgRaw, " ")),
      {priv_msg, #msg{from=From, channel=Channel, message=Msg} } ;
    Other ->
      error_logger:warning_msg("Got unexpected Request ~p~n", [Other]),
      {ignore}
  end.

