%%------------------------------------------------------------------------------
%%% File    : modk_server_SUITE.erl
%%% Author  : abonnefoy
%%% Description : ???
%%------------------------------------------------------------------------------

-module(mock_server_SUITE).
-compile(export_all).
-include("common_test.hrl").
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,10}}].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.
groups() -> [].

init_per_testcase(_TestCase, Config) ->
  {ok, _Pid} = mock_server:start(#server_state{port=1337, loop={?MODULE, dummy_loop}}),
  Config.

end_per_testcase(_TestCase, _Config) ->
  exit(whereis(mock_server),"Pang"),
  ok.

all() ->
    [test_mock_server].

%%------------------------------------------------------------------------------
%% TEST CASES
%%------------------------------------------------------------------------------

test_mock_server() ->
    [].

test_mock_server(_Config) ->
  Params = [ {active, false} ],
  {ok, Socket} = ssl:connect("localhost", 1337,  Params, 2000),
  ssl:send(Socket, "bar"),
  {ok, "ok"} = ssl:recv(Socket, 0) .

dummy_loop({Socket, _Pid}) ->
  case ssl:recv(Socket, 0) of
    {ok, "bar"} ->
      ssl:send(Socket,"ok"),
      ssl:close(Socket);
    {error, Reason} ->
      io:format("Error while receiving data: ~w~n", [Reason]),
      {ko, Reason}
  end.

