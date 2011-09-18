-module(test_irlang_client).
-include("common_test.hrl").

setup() -> 
  ssl:start(),
  {ok, _MockServer} = mock_server:start_link(#server_state{port=1337, loop={?MODULE, check_connection}}),
  {ok, _Pid} = irlang_app:start(temporary, [1337, "localhost" ] ),
  %application:start(sasl),
  ok.

cleanup(_Pid) -> 
  irlang_app:shutdown(),
  ok.

check_client_registering(Socket) -> 
  ?assertSasl({ok, "NICK GA\r\n"}, ssl:recv(Socket, 0)),
  ?assertSasl({ok, "USER GA 0 * :GA\r\n"}, ssl:recv(Socket, 0)),
  ?assertSasl({ok, "JOIN #geek\r\n"}, ssl:recv(Socket, 0)).

check_connection({ Socket }) -> 
  check_client_registering(Socket),
  ssl:close(Socket).

check_ping({Socket}) -> 
  check_client_registering(Socket),
  ssl:send(Socket, irlang_request:ping("TOTOLOL")),
  ?assertSasl({ok, "PONG: TOTOLOL\r\n"}, ssl:recv(Socket, 0)),
  ssl:close(Socket).

test_connect_server() -> fun() ->
      "" = gen_fsm:sync_send_event(irlang_client, test_join())
  end.

test_ping() -> fun() ->
      %gen_server:call(mock_server, {change_loop, {?MODULE, check_ping} } )
      %, irlang_app:start(temporary, [1337, "localhost"] )
      ok
  end.

generator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_connect_server(),
      test_ping()
    ]
  }.

