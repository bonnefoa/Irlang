-module(test_irlang_client).
-include("common_test.hrl").

setup() -> 
  mock_server:start_link(#server_state{port=1337, loop={?MODULE, dummy_loop}}),
  irlang_app:start(temporary, [1337, "localhost", "#geek", "GA", "GA"] ),
  ok.

cleanup(_Pid) -> 
  ok.

test_connect_server() -> 
  %irlang_client:connect(test_server())
  ok
  .

dummy_loop(Data) -> 
  io:format("Dummy server :: Received  ~p", [Data]), 
  "ok"
  .

generator_test_() ->
  ssl:start(),
  application:start(sasl),
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      fun() -> test_connect_server() end
    ]
  }.

