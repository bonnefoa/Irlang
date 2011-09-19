-module(test_irlang_client).
-include("common_test.hrl").

setup() -> 
  ssl:start(),
  application:start(sasl),
  %{ok, _MockServer} = mock_server:start_link(#server_state{port=1337, loop={?MODULE, check_connection}}),
  %{ok, _Pid} = irlang_app:start(temporary, [1337, "localhost" ] ),
  ok.

cleanup(_Pid) -> 
  %irlang_app:shutdown(),
  %exit(whereis(mock_server),"Pang"),
  ok.

run_servers(Loop, Action) -> 
%  process_flag(trap_exit, true), 
%  try 
    %{ok, _MockServer} = mock_server:start_link(#server_state{port=1337, loop={?MODULE, Loop}}),
    %{ok, _Sup} = irlang_app:start(temporary, [1337, "localhost" ] ),
    Action().
    %exit(MockServer,"Pang")
%  catch 
%    _:_ -> ok
%  end .
%

check_connection(Pid) -> fun({ Socket }) -> 
      try
        {ok, "NICK GA\r\n"} = ssl:recv(Socket, 0),
        {ok, "USER GA 0 * :GA\r\n"} = ssl:recv(Socket, 0),
        {ok, "JOIN #geek\r\n"} = ssl:recv(Socket, 0)
      catch
        _:Reason -> Pid ! Reason
      end,
      Pid ! ok,
      ssl:close(Socket)
  end.

test_connect_server() -> fun() ->
      {ok, _MockServer} = mock_server:start_link(#server_state{port=1337, loop={?MODULE, check_connection}}),
      {ok, _Sup} = irlang_bot_server:start(#irc_server{port=1337, address="localhost"} ),
      ok = irlang_bot_server:join(test_record_join()),
      receive
        ok -> ok;
        Other -> 
          io:format("Got Other == ~p", [Other]),
          throw(Other)
      end
  end.

%check_ping({Socket}) -> 
  %check_client_registering(Socket),
  %ssl:send(Socket, irlang_request:ping("TOTOLOL")),
  %?assertSasl({ok, "PONG: TOTOLOL\r\n"}, ssl:recv(Socket, 0)),
  %ssl:close(Socket).

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
      test_connect_server()
      %test_ping()
    ]
  }.

