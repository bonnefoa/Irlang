-module(test_mock_server).
-include("common_test.hrl").

setup() -> 
  ok.

cleanup(_Pid) -> 
  ok.

test_mock_server() -> fun() -> 
      %{ok, MockServer} = mock_server:start_link(#server_state{port=1338, loop={?MODULE, dummy_loop}}),
      Params = [ {active, false} ],
      {ok, Socket} = ssl:connect("localhost", 1338,  Params, 2000),
      ssl:send(Socket, "bar"),
      {ok, "ok"}= ssl:recv(Socket, 0)
  end .

dummy_loop({Socket}) ->
  case ssl:recv(Socket, 0) of
    {ok, "bar"} ->
      ssl:send(Socket,"ok"),
      ssl:close(Socket);
    {error, Reason} ->
      io:format("Error while receiving data: ~w~n", [Reason]),
      {ko, Reason}
  end.

generator_test_() ->
  ssl:start(),
  application:start(sasl),
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_mock_server()
    ]
  }.


