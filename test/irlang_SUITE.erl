%%------------------------------------------------------------------------------
%%% File    : irlang_SUITE.erl
%%% Author  : abonnefoy
%%% Description : ???
%%------------------------------------------------------------------------------

-module(irlang_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("common_test.hrl").

suite() -> [{timetrap,{minutes,1}}].
init_per_suite(Config) ->
  ssl:start(),
  Config.
end_per_suite(_Config) -> ok.
init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.
groups() -> [].

init_per_testcase(_TestCase, Config) ->
  {ok, _MockServer} = mock_server:start_link(#server_state{port=1337, loop={?MODULE, check_ping}, pid=self()}),
  {ok, _Sup} = irlang_app:start(temporary, [1337, "localhost"]),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

all() ->
    [
      %test_connect_server
      test_ping
    ].

%%------------------------------------------------------------------------------
%% TEST CASES
%%------------------------------------------------------------------------------

check_connection({Socket, Pid}) -> 
  try
    {ok, "NICK irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "USER irlang 0 * :irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "JOIN #geek\r\n"} = ssl:recv(Socket, 0, 500)
  catch
    _:Reason -> Pid ! Reason
  end,
  Pid ! ok,
  ssl:close(Socket) .

test_connect_server(_Config) ->
  gen_server:call(mock_server, {change_loop, {?MODULE, check_connection} } ),
  ok = irlang_bot_server:join(test_record_join()),
  wait_ok().

%% ===================================================================
%% Check ping
%% ===================================================================

check_ping({Socket, Pid}) ->
  process_flag(trap_exit, true),
  try
    {ok, "NICK irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "USER irlang 0 * :irlang\r\n"} = ssl:recv(Socket, 0, 500),
    {ok, "JOIN #geek\r\n"} = ssl:recv(Socket, 0, 500),
    ssl:send(Socket, irlang_request:ping("TOTOLOL"))
    , {ok, "PONG: TOTOLOL\r\n"} = ssl:recv(Socket, 500)
  catch
    _:Reason -> Pid ! Reason
  end,
  Pid ! ok,
  ssl:close(Socket) .

test_ping(_Config) ->
  ok = gen_server:call(mock_server, {change_loop, {?MODULE, check_ping} } ),
  ok = irlang_bot_server:join(test_record_join()),
  wait_ok().


%% ===================================================================
%% Private function
%% ===================================================================

wait_ok() ->
  receive
    ok -> ok;
    Other ->
      io:format("Got Other == ~p", [Other]),
      throw(Other)
  end.

