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
    socket
  } ).


