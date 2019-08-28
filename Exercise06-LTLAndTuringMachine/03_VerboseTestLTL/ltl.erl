-module(ltl).
-export([prop/1,neg/1,conj/2,disj/2,x/1,f/1,g/1,u/2,r/2,w/2, fn/2,
         start/0, showLTL/1, step/1,
         assert/1,newProp/1,releaseProp/1,status/0]).

%I am skipping the implemenation of path steps.. 
%Bassically writing for Part a
start() -> LTL = spawn(fun() -> ltl([],[],[]) end),
           register(ltl,LTL).

ltl(Phis,Asserts,Props) ->
  receive
    {assert,Phi} -> base:printLn("Got new Assert"),
                    base:printLn(showLTL(normalize(Phi))),
                    case check(normalize(Phi),Props) of
                      true  -> ltl(Phis,Asserts,Props);
                      false -> ptong(Phi);
                      Phi1  -> ltl([Phi1|Phis],[Phi|Asserts],Props)
                    end;
    {newProp,P} ->
      case lists:member(P,Props) of
        true -> ltl(Phis,Asserts,Props);
        false ->
          NewProps = [P|Props],
          lists:map(fun(Phi) -> base:printLn("newProp step x : " ++ showLTL(step(Phi))) end, Phis),
          lists:map(fun(Phi) -> base:printLn("newProp check x : " ++ showLTL(check(step(Phi),NewProps))) end, Phis),
          Phis1 = lists:map(fun(Phi) -> check(step(Phi),NewProps) end, Phis),
          {Phis2,Asserts2} = analyze(Phis1,Asserts),
          ltl(Phis2,Asserts2,NewProps)
      end;
    {releaseProp,P} ->
      NewProps = lists:delete(P,Props),
      lists:map(fun(Phi) -> base:printLn("releaseProp step x : " ++ showLTL(step(Phi))) end, Phis),
      Phis1 = lists:map(fun(Phi) -> check(step(Phi),NewProps) end, Phis),
      {Phis2,Asserts2} = analyze(Phis1,Asserts),
      ltl(Phis2,Asserts2,NewProps);
    status ->
      base:printLn("Unevaluated Assertions:"),
      lists:zipwith(fun(Phi,Assert) ->
                      base:printLn(showLTL(Assert)),
                      base:printLn("  "++showLTL(Phi)) end,
                    Phis,Asserts),
      ltl(Phis,Asserts,Props)
  end.

assert(Phi)    -> ltl!{assert,Phi}.
newProp(P)     -> ltl!{newProp,P}.
releaseProp(P) -> ltl!{releaseProp,P}.
status()       -> ltl!status.  

ptong(Phi) -> base:printLn("Assertion violated: "++showLTL(Phi)),
              base:getLn("Continue: ").

analyze([],[]) -> {[],[]};
analyze([true|Phis], [_|Asserts]) -> analyze(Phis,Asserts);
analyze([false|Phis],[A|Asserts]) -> ptong(A), analyze(Phis,Asserts);
analyze([Phi|Phis],  [A|Asserts]) ->
    {Phis1,Asserts1} = analyze(Phis,Asserts),
    {[Phi|Phis1],[A|Asserts1]}.

prop(P)       -> {prop,P}.
neg(Phi)      -> {neg,Phi}.
disj(Phi,Psi) -> {disj,Phi,Psi}.
conj(Phi,Psi) -> {conj,Phi,Psi}.
x(Phi)        -> {x,Phi}.
f(Phi)        -> f(0,Phi).
g(Phi)        -> g(0,Phi).
u(Phi,Psi) -> {u,Phi,Psi}.
r(Phi,Psi) -> {r,Phi,Psi}.
w(Phi,Psi) -> {w,Phi,Psi}.
fn(N,Phi) -> {fn,N,Phi}.

f(Cnt,Phi) -> {f,Cnt,Phi}.
g(Cnt,Phi) -> {g,Cnt,Phi}.

showLTL({prop,P}) -> base:show(P);
showLTL({neg,Phi}) -> "(neg "++showLTL(Phi)++")";
showLTL({disj,Phi,Psi}) -> "("++showLTL(Phi)++" or "++showLTL(Psi)++")";
showLTL({conj,Phi,Psi}) -> "("++showLTL(Phi)++" and "++showLTL(Psi)++")";
showLTL({x,Phi}) -> "(X"++showLTL(Phi)++")";
showLTL({f,Phi}) -> "(F"++showLTL(Phi)++")";
showLTL({g,Phi}) -> "(G"++showLTL(Phi)++")";
showLTL({u,Phi,Psi}) -> "("++showLTL(Phi)++" U "++showLTL(Psi)++")";
showLTL({r,Phi,Psi}) -> "("++showLTL(Phi)++" R "++showLTL(Psi)++")";
showLTL({w,Phi,Psi}) -> "("++showLTL(Phi)++" W "++showLTL(Psi)++")";
showLTL({fn,N,Phi}) -> "FN"++base:show(N)++showLTL(Phi);
showLTL({f,Cnt,Phi}) -> "F"++base:show(Cnt)++showLTL(Phi);
showLTL({g,Cnt,Phi}) -> "G"++base:show(Cnt)++showLTL(Phi);
showLTL(Phi) -> base:show(Phi).

normalize(true)           -> true;
normalize(false)          -> false;
normalize({prop,P})       -> prop(P);
normalize({conj,Phi,Psi}) -> conj(normalize(Phi),normalize(Psi));
normalize({disj,Phi,Psi}) -> disj(normalize(Phi),normalize(Psi));
normalize({x,Phi})        -> x(normalize(Phi));
normalize({f,Phi})        -> f(normalize(Phi));
normalize({g,Phi})        -> g(normalize(Phi));
normalize({u,Phi,Psi}) -> u(normalize(Phi),normalize(Psi));
normalize({r,Phi,Psi}) -> r(normalize(Phi),normalize(Psi));
normalize({w,Phi,Psi}) -> w(normalize(Phi),normalize(Psi));
normalize({fn,N,Phi}) -> fn(N,normalize(Phi));
normalize({f,Cnt,Phi}) -> f(Cnt,normalize(Phi));
normalize({g,Cnt,Phi}) -> g(Cnt,normalize(Phi));
normalize({neg,true})           -> false;
normalize({neg,false})          -> true;
normalize({neg,{prop,P}})       -> neg(prop(P));
normalize({neg,{neg,Phi}})      -> normalize(Phi);
normalize({neg,{conj,Phi,Psi}}) -> disj(normalize(neg(Phi)),
                                        normalize(neg(Psi)));
normalize({neg,{disj,Phi,Psi}}) -> conj(normalize(neg(Phi)),
                                        normalize(neg(Psi)));
normalize({neg,{u,Phi,Psi}}) -> r(normalize(neg(Phi)),normalize(neg(Psi)));
normalize({neg,{r,Phi,Psi}}) -> u(normalize(neg(Phi)),normalize(neg(Psi)));
normalize({neg,{w,Phi,Psi}}) -> normalize(neg(disj(u(Phi,Psi),g(Phi)))); 
                                % Because: Phi W Psi  =   ( Phi U Psi ) or G(Phi)
normalize({neg,{x,Phi}})        -> x(normalize(neg(Phi)));
normalize({neg,{f,Phi}})        -> g(normalize(neg(Phi)));
normalize({neg,{g,Phi}})        -> f(normalize(neg(Phi)));
normalize({neg,{fn,0,Phi}}) -> normalize(neg(Phi));
normalize({neg,{fn,N,Phi}}) -> conj(normalize(neg(Phi)),normalize(neg(x(fn(N-1,Phi)))));
normalize({neg,{f,Cnt,Phi}}) -> g(Cnt,normalize(neg(Phi)));
normalize({neg,{g,Cnt,Phi}}) -> f(Cnt,normalize(neg(Phi))).

check(true,_Props)  -> true;
check(false,_Props) -> false;
check({prop,P},Props)       -> lists:member(P,Props);
check({neg,{prop,P}},Props) -> not(check(prop(P),Props));
check({conj,Phi,Psi},Props) ->
  case check(Phi,Props) of
    true  -> check(Psi,Props);
    false -> false;
    Phi1  -> case check(Psi,Props) of
               false -> false;
               true  -> Phi1;
               Psi1  -> conj(Phi1,Psi1)
             end
  end;
check({disj,Phi,Psi},Props) ->
  case check(Phi,Props) of
    true  -> true;
    false -> check(Psi,Props);
    Phi1  -> case check(Psi,Props) of
               true  -> true;
               false -> Phi1;
               Psi1  -> disj(Phi1,Psi1)
             end
  end;
check({x,Phi},_Props) -> x(Phi);
check({f,Phi}, Props) -> check(disj(Phi,x(f(Phi))),Props);
check({g,Phi}, Props) -> base:printLn(showLTL(conj(Phi,x(g(Phi))))), check(conj(Phi,x(g(Phi))),Props);
check({u,Phi,Psi},Props) -> check(disj(Psi,conj(Phi,x(u(Phi,Psi)))),Props);
check({w,Phi,Psi},Props) -> check(disj(Psi,conj(Phi,x(w(Phi,Psi)))),Props);
check({r,Phi,Psi},Props) -> check(conj(Psi,disj(Phi,x(r(Phi,Psi)))),Props);
check({fn,0,Phi},Props) -> check(Phi,Props);
check({fn,N,Phi},Props) -> check(disj(Phi,x(fn(N-1,Phi))),Props);
check(Phi,_Props) -> base:putStrLn("Unexpected formula in check: "++
                                   showLTL(Phi)).

step({x,Phi}) -> Phi;
step({conj,Phi,Psi}) -> conj(step(Phi),step(Psi));
step({disj,Phi,Psi}) -> disj(step(Phi),step(Psi));
step(Phi) -> base:printLn("Unexpected formula in step: "++
                           showLTL(Phi)).






