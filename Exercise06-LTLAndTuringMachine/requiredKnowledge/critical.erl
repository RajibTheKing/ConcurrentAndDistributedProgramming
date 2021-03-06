-module(critical).
-export([start/0]).
-import(ltl,[prop/1,neg/1,conj/2,disj/2,x/1,f/1,g/1,
             assert/1,newProp/1,releaseProp/1,status/0]).

start() -> ltl:start(),
           assert(g(neg(conj(prop(csInc),prop(csDec))))),
           S = spawn(fun() -> store(42) end),
           spawn(fun() -> inc(S) end),
           dec(S).

store(V) ->
  base:printLn("How!!!! " ++ base:show(V)),
  receive
    {lookup,inc, P} -> base:printLn("returing to inc: " ++ base:show(V)), P!V, store(V);
    {lookup,dec, P} -> base:printLn("------------------Dec return: " ++ base:show(V)), P!V, store(V);
    {set,V1}   -> store(V1)
  end.

inc(S) -> S!{lookup, inc, self()},
          receive
            V -> base:printLn("Inc: " ++ base:show(V)), newProp(csInc), S!{set,V+1}
          end,
          releaseProp(csInc),
          inc(S).

dec(S) -> S!{lookup,dec,self()},
          receive
            V -> base:printLn("Dec: " ++ base:show(V)), newProp(csDec), S!{set,V-1}
          end,
          releaseProp(csDec),
          dec(S).


%main() -> ltl:start(),
%          assert(f(teminated)),
%          simulate_tm(""),
%          newProp(termiated).
%

