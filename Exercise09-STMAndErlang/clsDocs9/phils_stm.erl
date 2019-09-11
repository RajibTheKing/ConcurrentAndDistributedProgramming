-module(phils_stm).
-export([main/0]).
-import(stm,[atomically/1,new_tvar/1,read_tvar/1,write_tvar/2,retry/0,or_else/2]).

main() ->
  S1 = atomically(fun() -> new_stick() end),
  S2 = atomically(fun() -> new_stick() end),
  S3 = atomically(fun() -> new_stick() end),
  S4 = atomically(fun() -> new_stick() end),
  S5 = atomically(fun() -> new_stick() end),
  spawn(fun() -> phil(S1,S2,1,500) end),
  spawn(fun() -> phil(S2,S3,2,500) end),
  spawn(fun() -> phil(S3,S4,3,500) end),
  spawn(fun() -> phil(S4,S5,4,500) end),
  base:getLn(),
  phil(S5,S1,5,500).
 
new_stick() -> new_tvar(true).

take_stick(Stick) ->
  Available = read_tvar(Stick),
  case Available of
    true -> write_tvar(Stick, false);
    false -> retry()
  end.

put_stick(Stick) ->
  write_tvar(Stick,true).

phil(Sl,Sr,Nr,0) -> base:printLn("Ready: "++base:show(Nr)),base:getLn(),
                    phil(Sl,Sr,Nr,5);
phil(Sl,Sr,Nr,N) ->
  base:printLn(base:show(Nr) ++ " is thinking"),
  atomically(fun() -> take_stick(Sl),
                      take_stick(Sr)
             end),
  %case atomically(fun() -> %take_stick(Sl)
  %                    or_else(fun() -> take_stick(Sr), true end,
  %                            fun() -> put_stick(Sl), false end)
  %                end) of
  %  false -> phil(Sl,Sr,Nr);
  %  true -> 
  base:printLn(base:show(Nr) ++ " is eating"),
  atomically(fun() -> put_stick(Sl),put_stick(Sr) end),
  phil(Sl,Sr,Nr,N-1)
  %end
  .
