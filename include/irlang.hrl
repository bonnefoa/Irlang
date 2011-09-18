-record(irc_server, {
   port,
   address
  }).

-record(join, {
   channel,
   nick,
   real_name
  }).

-record(fsm_state, { 
    message
  } ).

-record(bot_server_state, { 
    socket
  } ).


