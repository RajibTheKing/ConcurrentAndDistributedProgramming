-module(stm).
-export([atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0, or_else/2, testLock/0]).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1]).

new_tvar(Value) ->
  spawn(fun() -> tvar(Value,0,[]) end).
  

tvar(Value,Version,Susps) -> 
  receive
    {read,Pid} -> Pid!{value,Value,Version},
                  tvar(Value,Version,Susps);
    {lock,Pid} -> Pid!locked,
                  tvar_locked(Value,Version,Susps)
  end.

tvar_locked(Value,Version,Susps) ->
  receive
    {read,Pid} -> Pid!{value,Value,Version},
                  tvar_locked(Value,Version,Susps);
    {write,New_value} -> 
        lists:map(fun(Susp) -> Susp!modified end,Susps),
        tvar_locked(New_value,Version + 1,[]);
    {new_susp,Susp} -> tvar_locked(Value,Version,[Susp|Susps]);
    unlock -> tvar(Value,Version,Susps)
  end.

core_read(TVar) ->
  TVar!{read,self()},
  receive
    {value,V,Version} -> {value,V,Version}
  end.

core_write(TVar,Value) -> TVar!{write,Value}.

lock(TVar) ->
  TVar!{lock,self()},
  receive
    locked -> locked
  end.

unlock(TVar) ->
  TVar!unlock.

%TVar and A process wchich is suspending this TVar
susp(TVar,P) ->
  TVar ! {new_susp,P}.


read_tvar(TVar) ->
  {RS,WS} = get(state),
  case lookup(TVar,WS) of
    none ->
      {value,V,Version} = core_read(TVar),
      case lookup(TVar,RS) of
        none            -> put(state,{insert(TVar,Version,RS),WS}),
                           V;
        {value,Version} -> V;
        {value,_}       -> throw(rollback)
      end;
    {value,V} -> V 
  end.

write_tvar(TVar,Value) ->
  {RS,WS} = get(state),
  put(state,{RS,enter(TVar,Value,WS)}),
  ok.

retry() -> throw(retry).


%OrElse: The transaction s1 ‘orElse‘ s2 first runs s1; if it retries, then
%  s1 is abandoned with no effect, and s2 will execute. If s2 retries as well,
%  the entire call retries — but it waits on the variables read by either
%  of the two nested transactions.


or_else(T1,T2) ->
  {_,WS0} = get(state),
  case catch T1() of
    rollback -> throw(rollback);
    retry -> {RS1,_WS1} = get(state),
             put(state,{RS1,WS0}),  % I don't know why I am using updated RS1. 
             T2();                  % Do I need to catch retry for the second Transactions as well?? This is ambiguous
    Res -> Res
  end.

atomically(Trans) ->
  put(state,{empty(),empty()}),
  case catch Trans() of
    rollback -> atomically(Trans);
    retry    -> {RS,_} = get(state),
                TVars = keys(RS),
                lock_l(TVars),
                case validate(to_list(RS)) of
                  true -> susps_l(TVars),
                          unlock_l(TVars),
                          receive
                            modified -> base:print("AWAKEN "), ok
                          end;
                  false -> unlock_l(TVars)
                end,
                atomically(Trans);
    Res      -> {RS,WS} = get(state),
                TVars = lists:umerge(keys(RS),keys(WS)),
                lock_l(TVars),
                case validate(to_list(RS)) of
                  true  -> commit(to_list(WS)),
                           unlock_l(TVars),
                           Res;
                  false -> unlock_l(TVars),
                           atomically(Trans)
                end
  end.

lock_l(TVars) -> 
  lists:map(fun(TVar) -> lock(TVar) end, TVars).

unlock_l(TVars) -> lists:map(fun(TVar) -> unlock(TVar) end, TVars).

susps_l(TVars) -> Me = self(),
                  P = spawn(fun() ->
                              receive
                                modified -> Me ! modified
                              end
                            end),
                  lists:map(fun(TVar) -> susp(TVar,P) end, TVars).
 
validate([])            -> true;
validate([{TVar,Version}|RL]) ->
  case core_read(TVar) of
    {value,_,Version} -> validate(RL);
    _                 -> false
  end.

commit(WL) -> lists:map(fun({TVar,V}) -> core_write(TVar,V) end, WL).




otherProcess(Tvar) ->
  lock(Tvar),
  core_write(Tvar, 15),
  NowValue = core_read(Tvar),
  base:printLn(base:show(NowValue) ++ " Now Changed value from otherProcess\n"),
  unlock(Tvar).



testLock() ->
  TvarPid = new_tvar(10),
  lock(TvarPid),
  spawn(fun() -> otherProcess(TvarPid) end),
  TestValue = core_read(TvarPid),
  base:printLn(base:show(TestValue) ++ " Now Changed value from Main process\n"),
  unlock(TvarPid).



















