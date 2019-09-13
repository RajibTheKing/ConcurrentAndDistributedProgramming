-module(stmtest).

% main interface
-export([run/7]).

% other functions
-export([minus_one/1, plus_one/1, times_to/1,test/0]).

% version with monitor
-import(stmmon, [atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0]).

% version without monitor
%-import(stm, [atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0]).

run(Procs, Vars, Times, Reads, Writes, Reading, Writing) ->
  register(main, self()), % Global verfügbare Pid.

  % Für Version mit Monitor
  stmmon:start_monitor(),

  TVars = atomically(fun() -> apply_many(Vars, fun() -> new_tvar(0) end) end),
  spawn_many(Procs,fun() ->act_many(Times,Reads,Writes,TVars,Vars,Reading,Writing) end),
  receive_many(Procs), % Warte, bis alle Prozesse fertig sind.
  Command = base:getLn(),
  Results = lists:zip(lists:seq(1, Vars), tvar_values(TVars)),
  process(Command, Results),
  unregister(main).

apply_many(0,_) -> [];
apply_many(Times,Fun) -> [Fun() | apply_many(Times-1,Fun)].

spawn_many(0,_) -> [];
spawn_many(Times,Fun) -> [spawn(Fun) | spawn_many(Times-1,Fun)].

receive_many(0) -> base:printLn(done);
receive_many(N) ->
  receive ready -> receive_many(N - 1) end.

process(Command, Results) ->
  case Command of
    "print" -> base:print(Results), process(base:getLn(), Results);
    "sum" -> base:print(lists:sum(lists:map(fun({_, Y}) -> Y end, Results))),
      process(base:getLn(), Results);
    "mon" -> monitor ! {monitor, self()},
      receive X -> base:print(X) end,
      process(base:getLn(), Results);
    "quit" -> ok;
    _ -> base:printLn("Your options are:"),
      base:printLn("print (variable bindings), sum (sum of all variables)," ++
        "mon (monitored effects), help (this message), quit (forfeit all" ++
        " knowledge of the simulation)"),
      process(base:getLn(), Results)
  end.


act_many(0, _Rs, _Ws, _Ts, _Vs, _Reader, _Writer)     -> main ! ready;
act_many(Times, Rs,  Ws,  Ts,  Vs,  Reader,  Writer) ->
  atomically(fun() -> act(Rs, Ws, Ts, Vs, Reader, Writer) end),
  act_many(Times - 1, Rs, Ws, Ts, Vs, Reader, Writer).


act(Rs, Ws, TVars, Vars, Reader, Writer) ->
  ToRead  = make_randoms(Rs, Vars),
  ToWrite = make_randoms(Ws, Vars),
  lists:map(fun(Pos) -> Reader(lists:nth(Pos, TVars)) end, ToRead),
  lists:map(fun(Pos) -> Writer(lists:nth(Pos, TVars)) end, ToWrite).

tvar_values(TVars) -> lists:map(fun(TVar) -> read_tvar(TVar) end, TVars).

make_randoms(N,UpTo) -> apply_many(N,fun() -> rand:uniform(UpTo) end).

modify(Fun, TVar) ->
  V = read_tvar(TVar),
  write_tvar(TVar, Fun(V)).


% Increment a a TVar by one
plus_one(TVar)  -> modify(fun(X) -> X + 1 end, TVar).

% Decrement a TVar by one
minus_one(TVar) -> modify(fun(X) -> X - 1 end, TVar).

% Multipy a TVar by 2
times_to(TVar) -> modify(fun(X) -> 2 * X end, TVar).

