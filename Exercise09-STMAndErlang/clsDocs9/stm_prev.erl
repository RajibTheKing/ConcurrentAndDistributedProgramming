-module(stm_prev).
-export([atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0]).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1]).

new_tvar(Value) ->
  spawn(fun() -> tvar(Value,0) end).

tvar(Value,Version) -> 
  receive
    {read,Pid} -> Pid!{value,Value,Version},
                  tvar(Value,Version);
    {lock,Pid} -> Pid!locked,
                  tvar_locked(Value,Version)
  end.

tvar_locked(Value,Version) ->
  receive
    {read,Pid} -> Pid!{value,Value,Version},
                  tvar_locked(Value,Version);
    {write,New_value} -> tvar_locked(New_value,Version + 1);
    unlock -> tvar(Value,Version)
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

atomically(Trans) ->
  base:printLn(base:show(Trans)),
  put(state,{empty(),empty()}),
  case catch Trans() of
    rollback -> atomically(Trans);
    retry    -> atomically(Trans);
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

lock_l(TVars) -> lists:map(fun(TVar) -> lock(TVar) end, TVars).

unlock_l(TVars) -> lists:map(fun(TVar) -> unlock(TVar) end, TVars).

validate([])            -> true;
validate([{TVar,Version}|RL]) ->
  case core_read(TVar) of
    {value,_,Version} -> validate(RL);
    _                 -> false
  end.

commit(WL) -> lists:map(fun({TVar,V}) -> core_write(TVar,V) end, WL).


















