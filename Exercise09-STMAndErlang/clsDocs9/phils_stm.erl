-module(phils_stm).
-export([main/0]).
-import(stm,[atomically/1,new_tvar/1,read_tvar/1,write_tvar/2,retry/0,or_else/2]).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1]).

main() ->
  S1 = atomically(fun() -> new_stick() end),
  S2 = atomically(fun() -> new_stick() end),
  S3 = atomically(fun() -> new_stick() end),
  S4 = atomically(fun() -> new_stick() end),
  S5 = atomically(fun() -> new_stick() end),
  spawn(fun() -> phil(S1,S2,1,50000) end),
  spawn(fun() -> phil(S2,S3,2,50000) end),
  spawn(fun() -> phil(S3,S4,3,50000) end),
  spawn(fun() -> phil(S4,S5,4,50000) end),
  base:getLn(),
  phil(S5,S1,5,50000).
 
new_stick() -> new_tvar(true).

take_stick(Stick) ->
  Available = read_tvar(Stick),
  case Available of
    true -> write_tvar(Stick, false);
    false -> retry();
    _ -> base:printLn("Something is wrong")
  end.

put_stick(Stick) ->
  write_tvar(Stick,true).

phil(_Sl,_Sr,Nr,0) -> base:printLn("Ready: "++base:show(Nr));
phil(Sl,Sr,Nr,N) ->
  base:printLn(base:show(Nr) ++ " is thinking" ++ "  " ++ base:show(N) ++ "\n"),
  %%atomically(fun() -> take_stick(Sl),
  %%                    take_stick(Sr)
  %%           end),

  atomically(fun() -> take_stick(Sl) end),
  atomically(fun() -> take_stick(Sr) end),


  %put(state,{empty(),empty()}),
  %take_stick(Sl),
  %put(state,{empty(),empty()}),
  %take_stick(Sr),


  %case atomically(fun() -> %take_stick(Sl)
  %                    or_else(fun() -> take_stick(Sr), true end,
  %                            fun() -> put_stick(Sl), false end)
  %                end) of
  %  false -> phil(Sl,Sr,Nr);
  %  true -> 


  base:printLn(base:show(Nr) ++ " is eating"),
  %%atomically(fun() -> put_stick(Sl),put_stick(Sr) end),

  atomically(fun() -> put_stick(Sl) end),
  atomically(fun() -> put_stick(Sr) end),

  %put(state,{empty(),empty()}),
  %put_stick(Sl),
  %put(state,{empty(),empty()}),
  %put_stick(Sr),


  phil(Sl,Sr,Nr,N-1)
  %end
  .
