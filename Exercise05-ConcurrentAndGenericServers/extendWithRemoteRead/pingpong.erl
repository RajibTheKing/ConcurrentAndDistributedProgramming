-module(pingpong).
-export([ping/0,start_ping/0]).
-import(remote_chan,[new_chan/0,chan_register/3,chan_lookup/2,read_chan/1,
                     write_chan/2,serialize/1]).

start_ping() -> Chan = new_chan(),
                PongChan = new_chan(),
                chan_register("localhost","ping",Chan),
                chan_register("localhost","pong",PongChan),
                ping_loop(Chan,PongChan,0).

ping_loop(Chan,PongChan,N) -> 
  case read_chan(Chan) of
    ping -> write_chan(PongChan,{pong,N}),
            ping_loop(Chan,PongChan,N+1)
  end.

ping() ->
  case chan_lookup("localhost","ping") of
    nothing -> base:printLn("ping-service not available");
    {just,Chan} -> write_chan(Chan,ping),
      case chan_lookup("localhost","pong") of
        nothing -> base:printLn("ping-service not available");
        {just,PongChan} ->
          case read_chan(PongChan) of
            {pong,N} -> {pong,N};
            Oth  -> base:printLn("Unknown readChan result: "++base:show(Oth))  
          end;
        Oth  -> base:printLn("Unknown chanLookup result: "++base:show(Oth))  
      end
  end.
          