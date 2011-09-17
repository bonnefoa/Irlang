%%%-------------------------------------------------------------------
%%% Author  : abonnefoy
%%% @doc The socket manager
%%% @end
%%%-------------------------------------------------------------------
-module(irlang_client).
-include("irlang.hrl").
-export([connect/1]).

connect(#server{port=Port, address=Address, channel=Channel, nick=Nick, real_name=RealName}) -> 
  Params = [ {active, true} ],
  io:format("Connecting ", []),
  {ok, Socket} = ssl:connect(Address, Port,  Params, 2000),
  ok = ssl:ssl_accept(Socket),
  ok = ssl:send(Socket, irlang_request:nick(Nick)),
  ok = ssl:send(Socket, irlang_request:user(Nick, RealName)),
  ok = ssl:send(Socket, irlang_request:join(Channel)),
  loop(Socket)
  .

loop(Socket) -> 
  receive
    ["PING " | Msg ] -> 
      io:format("Ping received", [ Msg ]),
      ok = ssl:send(Socket, irlang_request:pong(Msg));
    Msg -> 
      io:format("received ~p", [ Msg ])
  end,
  loop(Socket).

