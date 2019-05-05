-module(pingpong).
-export([ping/0,start_ping/0]).
-import(remote_chan,[new_chan/0,chan_register/3,chan_lookup/2,read_chan/1,
                     write_chan/2,serialize/1]).

start_ping() -> Chan = new_chan(),
                chan_register("localhost","ping",Chan),
                ping_loop(Chan,0).

ping_loop(Chan,N) -> 
  case read_chan(Chan) of
    {ping,Ans_chan} -> write_chan(Ans_chan,{pong,N}),
                       ping_loop(Chan,N+1)
  end.

ping() ->
  case chan_lookup("localhost","ping") of
    nothing -> base:printLn("ping-service not available");
    {just,Chan} -> Ans_chan = new_chan(),
                   write_chan(Chan,{ping,Ans_chan}),
                   case read_chan(Ans_chan) of
                     {pong,N} -> {pong,N};
                     Oth  -> base:printLn("Unknown readChan result: "++base:show(Oth))  
                   end;
    Oth  -> base:printLn("Unknown chanLookup result: "++base:show(Oth))  
  end.
                   
