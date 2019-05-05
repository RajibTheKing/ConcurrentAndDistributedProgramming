-module(semaphoreImpl).
-export([semaphore/1, newSemaphore/1, p/1, v/1, lookup/1]).

newSemaphore(N) -> spawn(fun() -> semaphore(N) end).

semaphore(0) ->
  receive
    {lookup,Pid} -> Pid!{v,0}, semaphore(0);
    release      -> semaphore(1)
  end;
semaphore(N) when N > 0 ->
  receive
    {acquire,Pid} -> Pid!acquired, semaphore(N-1);
    {lookup,Pid}  -> Pid!{v,N}, semaphore(N);
    release       -> semaphore(N+1)
  end.

p(S) ->
    S!{acquire, self()},
    receive 
      acquired -> ok 
    end.

v(S) -> 
    S!release.

lookup(S) ->
    S!{lookup, self()},
    receive 
        {v,V} -> V 
    end.