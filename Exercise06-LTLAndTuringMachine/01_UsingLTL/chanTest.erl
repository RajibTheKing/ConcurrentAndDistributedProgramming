-module(chanTest).
-export([start/0]).
-import(ltl,[prop/1,neg/1,conj/2,disj/2,x/1,f/1,g/1,
             assert/1,newProp/1,releaseProp/1,status/0]).
% LTL Property: G(neg isEmpty --> F(isEmpty)) 
%               also G(isEmpty --> F(neg isEmpty))
% After applying Logics: G(neg (neg isEmpty) or F(isEmpty))
%                      : G(isEpty or F(isEmpty))


start() ->
    ltl:start(),
    assert(g(disj(prop(isEmpty), f(prop(isEmpty))))),
    MVar = spawn(fun() -> mvarEmpty() end),
    spawn(fun() -> takerThread(MVar) end),
    spawn(fun() -> putterThread(MVar, 42) end),
    spawn(fun() -> emptyCheckerThread(MVar) end),
    MVar.


mvarEmpty() -> 
    receive
        {put, V} -> mvarFull(V);
        {isEmpty, Pid} -> Pid ! {true}, mvarEmpty()
    end.

mvarFull(V) ->
    receive
        {take, Pid} -> Pid ! {value, V}, mvarEmpty();
        {isEmpty, Pid} -> Pid ! {false}, mvarFull(V)
    end.


takerThread(MVarPid) ->
    Me = self(), 
    MVarPid ! {take, Me},
    receive
        {value, _V} -> ok
    end, 
    takerThread(MVarPid).


putterThread(MVarPid, V) ->
    MVarPid ! {put, V},
    putterThread(MVarPid, V+1).

emptyCheckerThread(MVarPid) ->
    Me = self(), 
    MVarPid ! {isEmpty, Me}, 
    receive
        {true} -> base:printLn("Currently is Empty"), releaseProp(isEmpty), emptyCheckerThread(MVarPid);
        {false} -> base:printLn("NOT Empty"), newProp(isEmpty), emptyCheckerThread(MVarPid)
    end.