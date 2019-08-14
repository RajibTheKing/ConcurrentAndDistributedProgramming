-module(actualSieve).
-export([generate/1]).

generate(N) ->
    base:printLn("Hello World " ++ base:show(N)),
    AllNumberList = allNumber(N, 2),
    PrimeList = primeSieve(AllNumberList),
    printAll(PrimeList, 1).



allNumber(0, _) -> [];
allNumber(N, Num) ->
    [Num | allNumber(N-1, Num+1)].



primeSieve([]) -> [];
primeSieve([X|Xs]) ->
    [X | primeSieve(lists:filter(fun(Y) -> Y rem X /= 0 end, Xs))].


printAll([], _)-> ok;
printAll([X|Xs], Idx) -> 
    base:printLn(base:show(Idx) ++ " --> Prime: " ++ base:show(X)),
    printAll(Xs, Idx+1).


