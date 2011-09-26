-module(test_irlang_request).
-include("common_test.hrl").

setup() -> 
  ok.

cleanup(_Pid) -> 
  ok.

test_ping_request() -> 
  fun() -> 
      {ping, "TOTOLOL"} = irlang_request:request_to_event("PING TOTOLOL\r\n")
  end .

test_priv_message() -> 
  fun() -> 
      ?assertEqual({priv_msg, #msg{from="gra", channel="#geek", message=":irlang: TOTOTLOL"}} 
        , irlang_request:request_to_event(":gra!~gra@toto.org PRIVMSG #geek :irlang: TOTOTLOL\r\n")
      )
  end .

generator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_ping_request(),
      test_priv_message()
    ]
  }.
