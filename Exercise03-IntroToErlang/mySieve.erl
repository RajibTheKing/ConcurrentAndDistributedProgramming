-module (mySieve).
-export ([gen/1]).

gen(N) ->
    base:printLn("Lets start generating Primes "),
    NextNumber = spawn(fun() -> nextNumber(2) end),
    base:printLn("AllNumber pid = " ++ base:show(NextNumber)),
    primeCollector(NextNumber, N, 1).


nextNumber(Num) ->
    receive
        {next, P} -> P!{prime, Num}, nextNumber(Num + 1)
    end.

primeFlow(CurPrime, ParentProcess) ->
    receive
        {next, Pid} -> Prim = getNext(CurPrime, ParentProcess),
                       Pid!{prime, Prim},
                       primeFlow(CurPrime, ParentProcess)
    end.

getNext(CurPrime, ParentProcess) ->
    Me = self(),
    ParentProcess!{next, Me},
    receive
        {prime, N} ->   %base:printLn("Received: " ++ base:show(N)),      
                        case (N rem CurPrime) of
                            0 ->getNext(CurPrime, ParentProcess);
                            _ -> N
                        end
    end.

primeCollector(_, 0, _) -> ok;

primeCollector(ToCall, UpTo, Idx) ->
    Me = self(),
    ToCall!{next, Me},
    receive
        {prime, N} ->
            base:printLn("prime: " ++ base:show(N) ++ " --> " ++ base:show(Idx)),
            NewProcess = spawn(fun() -> primeFlow(N, ToCall) end),
            primeCollector(NewProcess, UpTo-1, Idx+1)
    end.

