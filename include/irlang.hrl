-record(irc_server, {
   port,
   address
  }).

-record(join, {
   channel,
   nick,
   real_name
  }).

-record(bot_server_state, { 
    loop_pid
    , nick
    , channel
    , pv
  } ).

-record(bot_server_loop, { 
    socket
  } ).

-record(msg, {
  from,
  channel,
  message
  }).

