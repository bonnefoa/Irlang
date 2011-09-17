-record(server, {
   port,
   address,
   channel,
   nick,
   real_name
  }).

-record(state, {
  socket,
  connected,
  message 
  }).

