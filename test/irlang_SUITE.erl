%%------------------------------------------------------------------------------
%%% File    : irlang_SUITE.erl
%%% Author  : abonnefoy
%%% Description : ???
%%------------------------------------------------------------------------------

-module(irlang_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("common_test.hrl").

-define(WAIT_OK,
  receive
    ok -> ok;
    Other ->
      io:format("Got Other == ~p", [Other]),
      throw(Other)
  end
).

suite() -> [{timetrap,{minutes,10}}].
init_per_suite(Config) ->
  ssl:start(),
  Config.
end_per_suite(_Config) -> ok.
init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.
groups() -> [].

init_per_testcase(_TestCase, Config) ->
  {ok, _Sup} = irlang_app:start(temporary, [1337, "localhost"]),
  {ok, _MockServer} = mock_server:start_link(#server_state{port=1337, loop={?MODULE, check_connection}, pid=self()}),
  Config.

end_per_testcase(_TestCase, _Config) ->
  %exit(whereis(mock_server),"GRA"),
  %exit(whereis(irlang_sup),"GRA")
  ok.

all() ->
    [test_connect_server, test_ping].

%%------------------------------------------------------------------------------
%% TEST CASES
%%------------------------------------------------------------------------------

check_connection({Socket, Pid}) -> try
    {ok, "NICK irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "USER irlang 0 * :irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "JOIN #geek\r\n"} = ssl:recv(Socket, 0, 500)
  catch
    _:Reason -> Pid ! Reason
  end,
  Pid ! ok,
  ssl:close(Socket) .

test_connect_server() -> [].

test_connect_server(_Config) ->
  ok = irlang_bot_server:join(test_record_join()),
  ?WAIT_OK.


%% ===================================================================
%% Check ping
%% ===================================================================

check_ping({Socket, Pid}) ->
  try
    {ok, "NICK irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "USER irlang 0 * :irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "JOIN #geek\r\n"} = ssl:recv(Socket, 0, 500),
    ssl:send(Socket, irlang_request:ping("TOTOLOL")),
    {ok, "PONG: TOTOLOL\r\n"} = ssl:recv(Socket, 0)
  catch
    _:Reason -> Pid ! Reason
  end,
  Pid ! ok,
  ssl:close(Socket) .

test_ping() -> [].

test_ping(_Config) ->
  ok = irlang_bot_server:join(test_record_join()),
  ?WAIT_OK.

