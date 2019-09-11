-module(stmPes).
-export([atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0, or_else/2]).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1]).

new_tvar(Value) ->
  spawn(fun() -> tvar(Value,[]) end).

tvar(Value,Susps) -> 
  receive
    {read,Pid} -> Pid!{value,Value},
                  tvar_locked(Value,Susps);
    {lock,Pid} -> Pid ! locked,
                  tvar_locked(Value,Susps);
    {is_locked,Pid} -> Pid ! false, % add Pid to a new locklist
                       tvar(Value,Susps)
  end.

tvar_locked(Value,Susps) ->
  receive
    {read,Pid} -> Pid ! is_locked,
                  tvar_locked(Value,Susps);
    {write,New_value} ->
                  lists:map(fun(Pid) -> Pid ! modified end, Susps), 
                  tvar_locked(New_value,[]);
    {new_susp,Susp} ->
                  tvar_locked(Value,[Susp|Susps]);
    {lock,Pid} -> Pid ! lock_not_available,
                  tvar_locked(Value,Susps);
    unlock -> tvar(Value,Susps); % inform processes within locklist
    {is_locked,Pid} -> Pid ! true,
                       tvar_locked(Value,Susps)
  end.

%excercises:
%- Store information about locked resources in tvars instead of RS and WS.
%- Suspend rollback in readTVar and writeTVar until non-available tvar lock is released again

core_read(TVar) ->
  TVar!{read,self()},
  receive
    {value,V} -> {value,V};
    is_locked    -> is_locked
  end.

core_write(TVar,Value) -> TVar!{write,Value}.

lock(TVar) ->
  TVar ! {lock,self()},
  receive
    locked -> locked;
    lock_not_available -> lock_not_available
  end.

unlock(TVar) ->
  TVar!unlock.

susp(TVar,P) ->
  TVar ! {new_susp,P}.

read_tvar(TVar) ->
  {RS,WS} = get(state),
  case lookup(TVar,WS) of
    none ->
      case lookup(TVar,RS) of
        {value,V} -> V;
        none ->
          case core_read(TVar) of
            {value,V} -> put(state,{insert(TVar,V,RS),WS}),
                         V;
            is_locked -> throw(rollback)
          end
      end;
    {value,V} -> V
  end.

write_tvar(TVar,Value) ->
  {RS,WS} = get(state),
  case lookup(TVar,WS) of
    {value,_} -> ok;
    none -> case lookup(TVar,RS) of
              {value,_} -> ok;
              none -> case lock(TVar) of
                        lock_not_available -> throw(rollback);
                        locked -> ok 
                      end
            end
  end,
  put(state,{RS,enter(TVar,Value,WS)}).

retry() -> throw(retry).

or_else(T1,T2) ->
  {_,WS0} = get(state),
  case catch T1() of
    rollback -> throw(rollback);
    retry -> {RS1,_WS1} = get(state),
             put(state,{RS1,WS0}),
             T2();
    Res -> Res
  end.

atomically(Trans) ->
  put(state,{empty(),empty()}),
  case catch Trans() of
    rollback -> {RS,WS} = get(state),
                unlock_l(lists:umerge(keys(RS),keys(WS))),
                atomically(Trans);
    retry    -> {RS,WS} = get(state),
                RL = keys(RS),
                susps_l(RL),
                unlock_l(lists:umerge(RL,keys(WS))),
                receive
                  modified -> base:print("AWAKEN "),
                              ok
                end,
                atomically(Trans);
    Res      -> {RS,WS} = get(state),
                commit(to_list(WS)),
                unlock_l(lists:umerge(keys(RS),keys(WS))),
                Res
  end.

unlock_l(TVars) -> lists:map(fun(TVar) -> unlock(TVar) end, TVars).

susps_l(TVars) -> Me = self(),
                  P = spawn(fun() ->
                              receive
                                modified -> Me ! modified
                              end
                            end),
                  lists:map(fun(TVar) -> susp(TVar,P) end, TVars).

commit(WL) -> lists:map(fun({TVar,V}) -> core_write(TVar,V) end, WL).


















