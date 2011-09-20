%%------------------------------------------------------------------------------
%%% File    : irlang_fsm_SUITE.erl
%%% Author  : abonnefoy
%%% Description : ???
%%------------------------------------------------------------------------------

-module(irlang_fsm_SUITE).

-compile(export_all).

-include("common_test.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
  {ok, _Pid} = irlang_bot_fsm:start(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  exit(whereis(irlang_bot_fsm), quit),
  ok.

groups() ->
    [].

all() ->
    [test_join, test_disconnect].

%%------------------------------------------------------------------------------
%% TEST CASES
%%------------------------------------------------------------------------------

test_join() ->
    [].

test_join(_Config) ->
  {ok, [Nick, User, Join]} = gen_fsm:sync_send_event(irlang_bot_fsm, {join, test_record_join() } ),
  "NICK irlang\r\n" = Nick,
  "USER irlang 0 * :irlang\r\n" = User,
  "JOIN #geek\r\n" = Join.

test_disconnect() ->
    [].

test_disconnect(_Config) ->
  {ok, _} = gen_fsm:sync_send_event(irlang_bot_fsm, {join, test_record_join() } ),
  {ok, [ Cmd ]} = gen_fsm:sync_send_event(irlang_bot_fsm, {disconnect, "Graou" } ),
  "QUIT Graou\r\n" = Cmd.

