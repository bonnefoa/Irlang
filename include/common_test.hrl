-include_lib("eunit/include/eunit.hrl").
-include("irlang.hrl").
-compile(export_all).

-record( server_state, { lsocket, port, loop } ).

test_server() -> #server{ port=1337, address="midgard.adyxax.org", channel="#geek", nick="irlang", real_name="irlang" }.

