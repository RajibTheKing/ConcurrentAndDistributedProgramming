-module(sieve).
-export([firstNPrimes/1]).

% all numbers starting from N
allNumbers(N) ->
  io:fwrite("allNumbers(N): " ++ integer_to_list(N) ++ "~n", []),
  receive
    {next,P} -> io:fwrite("allNumbers(N) {next,P}: " ++ integer_to_list(N) ++ "~n", []),
                P!{num,N},
      allNumbers(N+1)
  end.

% ask previous sieve for next prime number
% start a new sieve process for next prime number
sieve(Prev) ->
  io:fwrite("sieve(Prev)~n", []),
  Prev!{next,self()},
  receive
    {num,N} -> io:fwrite("sieve(Prev) {num,N}: " ++ integer_to_list(N) ++ "~n", []),
               receive
                 {prim,Collector} ->
                   io:fwrite("sieve(Prev) {prim,Collector}~n", []),
                   Me = self(),
                   Child = spawn(fun() -> sieve(Me) end),
                   Collector!{prim,N,Child},
                   filter(N,Prev)
               end
  end.

% filter numbers with given prime number and
% send result to previous sieve process
filter(Prim,Prev) ->
  io:fwrite("filter(Prim,Prev)~n", []),
  receive
    {next,P} -> io:fwrite("filter(Prim,Prev) {next,P}~n", []),
                P!{num,getNext(Prim,Prev)},
      filter(Prim,Prev)
  end.

% compute next prime number
getNext(Prim,Prev) ->
  io:fwrite("getNext(Prim,Prev)~n", []),
  Prev!{next,self()},
  receive
    {num,N} -> io:fwrite("getNext(Prim,Prev) {num,N}: " ++ integer_to_list(N) ++ "~n", []),
               case (N rem Prim) of
                 0 -> getNext(Prim,Prev);
                 _ -> N
               end
  end.

% initialize collector
primes() ->
  io:fwrite("primes()~n", []),
  AllNumbers = spawn(fun() -> allNumbers(2) end),
  Sieve = spawn(fun() -> sieve(AllNumbers) end),
  collector(Sieve).

% collect prime numbers
collector(Sieve) ->
  io:fwrite("collector(Sieve)~n", []),
  receive
    {nextPrim,P} -> io:fwrite("collector(Sieve) {nextPrim,P}~n", []),
                    Sieve!{prim,self()},
      receive
        {prim,N,NextSieve} -> io:fwrite("collector(Sieve) {prim,N,NextSieve}: " ++ integer_to_list(N) ++ "~n", []),
                              P!{prim,N},
          collector(NextSieve)
      end
  end.

% return first N prim numbers
firstNPrimes(N) ->
  io:fwrite("StartingPoint: " ++ integer_to_list(N) ++ "~n", []),
  P = spawn(fun() -> primes() end),
  primeLoop(P,N).

primeLoop(_,0) -> [];
primeLoop(P,N) ->
  io:fwrite("primeLoop(P,N): " ++ integer_to_list(N) ++ "~n", []),
  P!{nextPrim,self()},
  receive
    {prim, Prim} -> io:fwrite("primeLoop(P,N) {prim, Prim}, GotPrim = " ++ integer_to_list(Prim) ++ "~n", []),
                    [Prim|primeLoop(P,N-1)]
  end.