-module(test_irlang_client).
-include("common_test.hrl").

setup() -> 
  ok.

cleanup(_Pid) -> 
  ok.

check_connection({ Socket }) -> 
  ?assertEqual({ok, "NICK GA\r\n"}, ssl:recv(Socket, 0)),
  ?assertEqual({ok, "USER GA 0 * :GA\r\n"}, ssl:recv(Socket, 0)),
  ssl:close(Socket).

test_connect_server() -> fun() ->
      {ok, MockServer} = mock_server:start(#server_state{port=1337, loop={?MODULE, check_connection}}),

      %Params = [ {reuseaddr, true} , {active, false} , {certfile,"/home/sora/git_repos/irlang/priv/certificate.pem"} , {keyfile, "/home/sora/git_repos/irlang/priv/key.pem"} ],
      %{ok, LSocket} = ssl:listen(1337, Params),
      %{ok, Socket} = ssl:transport_accept(LSocket),
      %ok = ssl:ssl_accept(Socket),

      irlang_app:start(temporary, [1337, "localhost", "#geek", "GA", "GA"] )
  end.

generator_test_() ->
  ssl:start(),
  application:start(sasl),
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_connect_server()
    ]
  }.

