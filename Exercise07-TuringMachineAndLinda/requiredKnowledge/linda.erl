-module(linda).
-export([start/0,test/0,test2/0,in/2,out/2,rd/2]).

start() -> tuple_space([],[]).

tuple_space(Data,Reqs) ->
  receive
    {out,Tuple} -> 
        {ResL,Keep,NewReqs} = requests_check(Tuple,Reqs),
        base:printLn(base:show(ResL)),
        lists:map(fun({MatchRes,CPid}) -> CPid!{linda_result,MatchRes} end,
                      ResL),
        case Keep of
          true  -> tuple_space(Data++[Tuple],NewReqs);
          false -> tuple_space(Data,NewReqs)
        end;
    {in,PatternFun,CPid} -> 
        {MRes,NewData} = pattern_check(PatternFun,Data),
        case MRes of
          nothing    -> tuple_space(Data,Reqs++[{PatternFun,CPid,in}]);
          {just,Res} -> CPid!{linda_result,Res},
                        tuple_space(NewData,Reqs)
        end;
    {rd,PatternFun,CPid} -> 
        {MRes,_} = pattern_check(PatternFun,Data),
        case MRes of
          nothing    -> tuple_space(Data,Reqs++[{PatternFun,CPid,rd}]);
          {just,Res} -> CPid!{linda_result,Res},
                        tuple_space(Data,Reqs)
        end
  end.

pattern_check(_,[]) -> {nothing,[]};
pattern_check(PatternFun,[Tuple|Data]) ->
  case catch PatternFun(Tuple) of
    {'EXIT',_} -> {MRes,NewData} = pattern_check(PatternFun,Data),
                  {MRes,[Tuple|NewData]};
    Res -> {{just,Res},Data}
  end.

requests_check(_,[]) -> {[],true,[]};
requests_check(Tuple,[{PatternFun,CPid,Mod}|Reqs]) ->
  case catch PatternFun(Tuple) of
    {'EXIT',_} -> {ResL,Keep,NewReqs} = requests_check(Tuple,Reqs),
                  {ResL,Keep,[{PatternFun,CPid, Mod}|NewReqs]};
    Res -> case Mod of
             in -> {[{Res,CPid}],false,Reqs};
             rd -> {ResL,Keep,NewReqs} = requests_check(Tuple,Reqs),
                   {[{Res,CPid}|ResL],Keep,NewReqs}
           end
  end.

in(Server,PatternFun) ->
  Server!{in,PatternFun,self()},
  receive
    {linda_result,Res} -> Res
  end.

rd(Server,PatternFun) ->
  Server!{rd,PatternFun,self()},
  receive
    {linda_result,Res} -> Res
  end.

out(Server,Tuple) ->
  Server!{out,Tuple}.

test() ->
  TS = spawn(fun() -> start() end),
  out(TS,{a,42}),
  base:printLn("Trying to Read {a,X}: " ++ base:show(rd(TS,fun({a,X}) -> X end))),
  out(TS,{a,42}),
  spawn(fun() -> base:printLn("Trying to Read {b, X}" ++ base:show(rd(TS,fun({b,X}) -> X end))) end),
  base:getLn(),
  base:printLn("executing out {b,73}"),
  out(TS,{b,73}),
  base:printLn("In111111: " ++ base:show(in(TS,fun({a,X}) -> X end))),
  base:printLn("In222222: " ++ base:show(in(TS,fun({a,X}) -> X end))),
  base:printLn("In333333: " ++ base:show(in(TS,fun({a,X}) -> X end))),

  base:printLn("In: " ++ base:show(in(TS,fun({b,X}) -> X end))).


test2() ->
  TS = spawn(fun() -> start() end),
  spawn(fun() -> base:printLn("Trying to Read {b, X}" ++ base:show(rd(TS,fun({b,X}) -> X end))) end),
  base:getLn(),
  spawn(fun() -> base:printLn("Trying to Read {b, X}" ++ base:show(rd(TS,fun({b,X}) -> X end))) end),
  base:getLn(),
  spawn(fun() -> base:printLn("Trying to Read {b, X}" ++ base:show(rd(TS,fun({b,X}) -> X end))) end),
  base:getLn(),
  spawn(fun() -> base:printLn("Trying to IN {b, X}" ++ base:show(in(TS,fun({b,X}) -> X end))) end),
  base:getLn(),
  base:printLn("executing out {b,73}"),
  out(TS,{b,73}).






