-module(genSieve).
-export([firstNPrime/1, init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

nextNumber(Num) ->
    receive
        {next, P} -> 
            P!{prime, Num}, 
            nextNumber(Num + 1)
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

init([]) ->
    base:printLn("--------------------Inside Init function"), 
    {ok, done}.

handle_call({parentProcess, ToCall}, _From, _State) ->
    Me = self(),
    ToCall ! {next, Me},
    receive
        {prime, N} -> 
            NewProcess = spawn(fun() -> primeFlow(N, ToCall) end),
            {reply, {N, NewProcess}, done}
    end.

handle_cast(_Msg, _State) -> not_implemented.

firstNPrime(N) ->
    ToCall = spawn(fun() -> nextNumber(2) end),
    base:printLn("I am starting with " ++ base:show(N)),
    gen_server:start_link( {local, mySieve}, ?MODULE, [], []),
    myloop(ToCall, N).


myloop(_ToCall, 0) -> ok;
myloop(ToCall, N) ->
    {V, Process} = gen_server:call(mySieve, {parentProcess, ToCall}),
    base:printLn("Here: " ++ base:show(V) ++ " and " ++ base:show(Process)),
    myloop(Process, N-1).

