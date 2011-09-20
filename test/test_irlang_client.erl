-module(test_irlang_client).
-include("common_test.hrl").
-define(WAIT_OK,
  receive
    ok -> ok;
    Other ->
      io:format("Got Other == ~p", [Other]),
      throw(Other)
  end
).

setup() ->
  ssl:start(),
  %application:start(sasl),
  {ok, _Sup} = irlang_app:start(temporary, [1337, "localhost"]),
  ok.

cleanup(_Pid) ->
  process_flag(trap_exit, true),
  try
    exit(whereis(mock_server),"GRA"),
    exit(whereis(irlang_sup),"GRA")
  catch
    throw:_ -> ok
  end.

run_servers(Loop, Action) ->
  process_flag(trap_exit, true),
  try
    ok = supervisor:terminate_child(irlang_sup, irlang_bot_server),
    {ok, _} = supervisor:restart_child(irlang_sup, irlang_bot_server),
    ok = supervisor:terminate_child(irlang_sup, irlang_bot_fsm),
    {ok, _} = supervisor:restart_child(irlang_sup, irlang_bot_fsm),
    {ok, MockServer} = mock_server:start_link(#server_state{port=1337, loop={?MODULE, Loop}, pid=self()}),
    Action(),
    exit(MockServer,"Pang")
  catch
    throw:_ -> ok
  end.

%% ===================================================================
%% Check connection server
%% ===================================================================

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

test_connect_server() -> fun() ->
      run_servers(check_connection, fun action_test_connect_server/0)
  end.

action_test_connect_server() ->
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

test_ping() -> fun() ->
      run_servers(check_ping, fun action_test_ping/0)
  end.

action_test_ping() ->
  ok = irlang_bot_server:join(test_record_join()),
  ?WAIT_OK.

%% ===================================================================
%% tests generator
%% ===================================================================

generator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_connect_server(),
      test_ping()
    ]
  }.

