-module(semaphoreManual).
-export([start/0]).

start() ->
    base:printLn("Hello World"),
    Semaphore = spawn(fun() -> newSemaphore(5) end),
    spawn(fun() -> p(Semaphore) end).

newSemaphore(0) -> 
    receive
        {release} -> newSemaphore(1)
    end;

newSemaphore(N) ->
    receive
        {acquire, P} -> base:printLn("Request to get semaphore"),
                        P!{acquired},
                        newSemaphore(N-1);
        {release} -> newSemaphore(N+1)
    end.

p(Semaphore) -> 
    Me = self(),
    Semaphore!{acquire, Me},
    receive
        {acquired} -> base:printLn("I got it")
    end.



