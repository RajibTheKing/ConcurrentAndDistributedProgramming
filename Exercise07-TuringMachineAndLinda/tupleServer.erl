-module(tupleServer).
-export([start/0,test/0,in/3,out/3,rd/3, testFunc/1]).

start() -> tuple_space([],[]).

tuple_space(Data,Reqs) ->
  receive
    {out,Tuple, TimeInfo} -> 
        {ResL,Keep,NewReqs} = requests_check({Tuple, TimeInfo},Reqs),
        lists:map(fun({MatchRes,CPid}) -> CPid!{linda_result,MatchRes} end,
                      ResL),
        case Keep of
          true  -> tuple_space([{Tuple,TimeInfo} | Data], NewReqs);
          false -> tuple_space(Data,NewReqs)
        end;
    {in,PatternFun, CPid, TimeInfo} -> 
        {MRes,NewData} = pattern_check(PatternFun,TimeInfo, Data),
        case MRes of
          nothing    -> tuple_space(Data,[{PatternFun,CPid,in, TimeInfo}|Reqs]);
          {just,Res} -> CPid!{linda_result,Res},
                        tuple_space(NewData,Reqs)
        end;
    {rd,PatternFun,CPid, TimeInfo} -> 
        {MRes,_} = pattern_check(PatternFun, TimeInfo, Data),
        case MRes of
          nothing    -> tuple_space(Data,[{PatternFun,CPid,rd, TimeInfo}|Reqs]);
          {just,Res} -> CPid!{linda_result,Res},
                        tuple_space(Data,Reqs)
        end
  end.

pattern_check(_, _, []) -> {nothing,[]};
pattern_check(PatternFun,ReqTimeInfo, [{Tuple, TimeInfo}|Data]) ->
  base:printLn("ReqTime: " ++ base:show(ReqTimeInfo)),
  base:printLn("TimeInfo: " ++ base:show(TimeInfo)),
  {ReqStart, _} = ReqTimeInfo,
  {_, TupleEnd} = TimeInfo,
  case TupleEnd < ReqStart of 
      true -> pattern_check(PatternFun, ReqTimeInfo, Data);
      false -> case catch PatternFun(Tuple) of
                 {'EXIT',_} -> {MRes,NewData} = pattern_check(PatternFun,ReqTimeInfo, Data),
                               {MRes,[{Tuple,TimeInfo}|NewData]};
                 Res ->   {{just,Res},Data}                          
               end
  end.

requests_check(_,[]) -> {[],true,[]};
requests_check({Tuple,TimeInfo},[{PatternFun,CPid,Mod,ReqTimeInfo}|Reqs]) ->
  {_, ReqEnd} = ReqTimeInfo,
  {TupleStart, _} = TimeInfo,
  case ReqEnd < TupleStart of
      true -> requests_check({Tuple,TimeInfo},Reqs);
      false ->
          case catch PatternFun(Tuple) of
              {'EXIT',_} -> {ResL,Keep,NewReqs} = requests_check({Tuple,TimeInfo},Reqs),
                            {ResL,Keep,[{PatternFun,CPid,Mod,ReqTimeInfo}|NewReqs]};
              Res -> case Mod of
                       in ->  {[{Res,CPid}],false,Reqs};
                           
                       rd -> {ResL,Keep,NewReqs} = requests_check({Tuple,TimeInfo},Reqs),
                              {[{Res,CPid}|ResL],Keep,NewReqs}
                     end
          end
  end.

in(Server,PatternFun, Timeout) ->
  TimeInfo = makeTimeInfo(Timeout),
  Server!{in, PatternFun, self(), TimeInfo},
  receive
    {linda_result,Res} -> Res
  after Timeout * 1000 -> nothing
  end.


rd(Server,PatternFun, Timeout) ->
  TimeInfo = makeTimeInfo(Timeout),
  Server!{rd,PatternFun,self(), TimeInfo},
  receive
    {linda_result,Res} -> Res
  after Timeout * 1000 -> nothing
  end.

out(Server,Tuple, Timeout) ->
  TimeInfo = makeTimeInfo(Timeout),
  Server!{out,Tuple, TimeInfo}.

getCurrentSec() ->
    {_, Secs, _} = erlang:timestamp(),
    Secs.

makeTimeInfo(Timeout) ->
  Secs = getCurrentSec(),
  Start = Secs,
  End = Secs + Timeout,
  {Start, End}.

testFunc(Timeout) ->
  receive
    {found,Rhd} -> {just,Rhd}
  after Timeout -> nothing
  end.

testCase1(ToExecute, LindaServer) ->
    receive {testing} -> ok
    after ToExecute * 1000 -> spawn(fun() -> base:printLn(in(LindaServer,fun({a,X}) -> X end, 5)) end)
    end.

testCase2(ToExecute, LindaServer) ->
    receive {testing} -> ok
    after ToExecute * 1000 ->  out(LindaServer, {a,42}, 10)
    end.

testCase3(ToExecute, LindaServer) ->
    receive {testing} -> ok
    after ToExecute * 1000 ->  spawn(fun() -> base:printLn(rd(LindaServer,fun({b,X}) -> X end, 20)) end)
    end.

testCase4(ToExecute, LindaServer) ->
    receive {testing} -> ok
    after ToExecute * 1000 ->  out(LindaServer, {d,13}, 15)
    end.

testCase5(ToExecute, LindaServer) ->
    receive {testing} -> ok
    after ToExecute * 1000 ->  out(LindaServer, {b,7}, 5)
    end.

testCase6(ToExecute, LindaServer) ->
    receive {testing} -> ok
    after ToExecute * 1000 ->  spawn(fun() -> base:printLn(in(LindaServer,fun({e,X}) -> X end, 2)) end)
    end.
%I am considering TimeOut as seconds
test() ->
  TS = spawn(fun() -> start() end),
  spawn(fun() -> testCase1(1, TS) end), % query = in {a,X} 5sec
  spawn(fun() -> testCase2(2, TS) end), % query = out {a, 42} 10sec
  spawn(fun() -> testCase3(10, TS) end), % query = rd {b, X} 20sec
  spawn(fun() -> testCase4(4, TS) end), % query = out {d, 13} 15sec
  spawn(fun() -> testCase5(5, TS) end), % query = out {b, 7} 5sec
  spawn(fun() -> testCase6(6, TS) end), % query = in {e, X} 2sec
  base:getLn().
