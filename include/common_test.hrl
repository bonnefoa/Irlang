-include_lib("eunit/include/eunit.hrl").
-include("irlang.hrl").
-compile(export_all).

-record( server_state, { lsocket, port, loop } ).
-define( assertSasl(Expected, Res), 
  case Expected =:= Res of
    true -> ok;
    false -> error_logger:error_msg(io_lib:format("Expected ~p, Got ~p~n", [Expected, Res])) 
  end
).

test_server() -> #irc_server{ port=1337, address="midgard.adyxax.org" }.
test_join() -> #join{ channel="#geek", nick="irlang", real_name="irlang" }.


