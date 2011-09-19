-module(test_irlang_fsm).
-include("common_test.hrl").

setup() -> 
  ok.

cleanup(_Pid) -> 
  ok.

run_fsm(Action) -> 
  {ok, Pid} = irlang_bot_fsm:start(test_record_server()),
  Action(),
  exit(Pid,"Pang") .

fun_test_join() -> 
  {ok, [Nick, User, Join]} = gen_fsm:sync_send_event(irlang_bot_fsm, {join, test_record_join() } ),
  ?assertEqual("NICK irlang\r\n", Nick),
  ?assertEqual("USER irlang 0 * :irlang\r\n", User),
  ?assertEqual("JOIN #geek\r\n", Join).

fun_test_disconnect() -> 
  {ok, _} = gen_fsm:sync_send_event(irlang_bot_fsm, {join, test_record_join() } ),
  {ok, [ Cmd ]} = gen_fsm:sync_send_event(irlang_bot_fsm, {disconnect, "Graou" } ),
  ?assertEqual("QUIT Graou\r\n", Cmd).

test_join() -> fun() -> run_fsm(fun fun_test_join/0) end.

test_disconnect() -> fun() -> run_fsm(fun fun_test_disconnect/0) end.

generator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_join(),
      test_disconnect()
    ]
  }.

