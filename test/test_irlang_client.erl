-module(test_irlang_client).
-include("common_test.hrl").

setup() -> ok.
cleanup(_Pid) -> ok.

test_connect_server() -> 
  irlang_client:connect(test_server()).

%server_state() ->
  %#server_state{
    %port        = 1234,
    %loop        = {admin_server_test, a_loop},
    %domain      = "http://localhost:1234/",
    %certificate = "misc/server/cert.pem",
    %private_key = "misc/server/privkey.pem",
    %password    = "root"
  %}.

%test_admin_server() -> fun() ->
      %{ok, Pid} = as_server:start(server_state()),
      %ping_socket(),
      %exit(Pid, test)
  %end.

%test_admin_sup() -> fun() ->
  %process_flag(trap_exit, true),
  %try
      %{ok, Pid} = as_sup:start_link([server_state()]),
      %ping_socket(),
        %exit(Pid, test)
      %catch
        %_:_ -> ok
      %end
  %end.

%test_admin_app() -> fun() ->
      %application:load(admin_server),
      %ping_socket()
  %end.

%ping_socket() ->
  %Params = [ {active, false} ],
  %{ok, Socket} = ssl:connect("localhost", 1234,  Params, 2000),
  %ssl:send(Socket, "bar"),
  %{ok, "ok"}= ssl:recv(Socket, 0).

%a_loop({Socket, _Domain}) ->
  %case ssl:recv(Socket, 0) of
    %{ok, Data} ->
      %io:format("Received (raw data): ~p~n", [Data]),
      %ssl:send(Socket,"ok"),
      %ssl:close(Socket);
    %{error, Reason} ->
      %io:format("Error while receiving data: ~w~n", [Reason]),
      %{ko, Reason}
  %end.

generator_test_() ->
  ssl:start(),
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      fun() -> test_connect_server() end
    ]
  }.

