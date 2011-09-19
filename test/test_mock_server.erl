-module(test_mock_server).
-include("common_test.hrl").

setup() -> 
  ok.

cleanup(_Pid) -> 
  ok.

run_mock(Action) -> 
  {ok, Pid} = mock_server:start(#server_state{port=1337, loop={?MODULE, dummy_loop}}),
  Action(),
  exit(Pid,"Pang") .

test_mock_server() -> fun() -> run_mock(fun fun_test_mock_server/0) end.

fun_test_mock_server() -> 
  Params = [ {active, false} ],
  {ok, Socket} = ssl:connect("localhost", 1337,  Params, 2000),
  ssl:send(Socket, "bar"),
  ?assertEqual( {ok, "ok"}, ssl:recv(Socket, 0) ).

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
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_mock_server()
    ]
  }.


