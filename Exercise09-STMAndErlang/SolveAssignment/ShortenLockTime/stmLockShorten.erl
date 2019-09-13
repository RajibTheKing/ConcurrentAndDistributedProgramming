-module(stmLockShorten).
-export([atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0]).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1]).

new_tvar(Value) -> spawn(fun() -> tvar(Value,0,[]) end).

tvar(Value,Version,Susps) ->
  receive
    {read,Pid} -> Pid!{value, Value, Version},
      tvar(Value,Version,Susps);
    {lock,Pid} -> Pid ! locked,
      tvar_locked(Value,Version,Susps)
  end.

tvar_locked(Value,Version,Susps) ->
  receive
    {read,Pid}        -> Pid!{value, Value, Version},
      tvar_locked(Value,Version,Susps);
    {write,New_value} -> lists:map(fun(Susp) -> Susp!{modified,self()} end,
      Susps),
      tvar_locked(New_value,Version+1,[]);
    {newSusp,Susp}    -> tvar_locked(Value,Version,[Susp|Susps]);
    unlock            -> tvar(Value,Version,Susps)
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

unlock(TVar) -> TVar!unlock.

susp(TVar,P) -> TVar!{newSusp,P}.

read_tvar(TVar) ->
  {RS,WS} = get(state),
  case lookup(TVar,WS) of
    none      -> {value,V,Version} = core_read(TVar),
      case lookup(TVar,RS) of
        none            -> put(state,{insert(TVar,Version,RS),WS}),
          V;
        {value,Version} -> V;
        _               -> throw(rollback)
      end;
    {value,V} -> V
  end.

write_tvar(TVar,Value) ->
  {RS,WS} = get(state),
  put(state,{RS,enter(TVar,Value,WS)}),
  ok.

retry() -> throw(retry).

atomically(Transaction) ->
  put(state,{empty(),empty()}),
  case catch Transaction() of
    rollback -> atomically(Transaction);
    retry    -> {RS,_} = get(state),
      TVars_k = keys(RS),
      TVars_v = to_list(RS),
      case lock_and_validate(TVars_k,TVars_v) of
        true -> susps_l(TVars_k),
          unlock_l(TVars_k),
          receive
            {modified,_TVar} -> ok
          end;
        false -> ok
      end,
      atomically(Transaction);
    Res      -> {RS,WS} = get(state),
      TVars = lists:umerge(keys(RS),keys(WS)),
      TVars_v = to_list(RS),
      case lock_and_validate(TVars,TVars_v) of
        true -> unlock_read_only(TVars_v,keys(WS)),
          commit(to_list(WS)),
          unlock_l(keys(WS)),
          Res;
        false -> atomically(Transaction)
      end
  end.

lock_l(TVars) -> lists:map(fun(TVar) -> lock(TVar) end, TVars).

unlock_l(TVars) -> lists:map(fun(TVar) -> unlock(TVar) end, TVars).

susps_l(TVars) -> Me = self(),
  P = spawn(fun() ->
    receive
      {modified,TVar} -> Me!{modified,TVar}
    end end),
  lists:map(fun(TVar) -> susp(TVar,P) end, TVars).

unlock_read_only([],_) -> "ok";
unlock_read_only(RL,[]) ->
  lists:map(fun({Var,_}) -> unlock(Var) end, RL);
unlock_read_only([{VR,_}|RL],WK) ->
  case lists:member(VR, WK) of
    true  -> unlock_read_only(RL,WK);
    false -> unlock(VR),
      unlock_read_only(RL,WK)
  end.

lock_and_validate([],_) -> true;
lock_and_validate([A|As],[{A,Version}|Rs]) ->
  lock(A),
  case validate(A,Version) of
    true ->
      case lock_and_validate(As,Rs) of
        true -> true;
        false -> unlock(A),
          false
      end;
    false -> unlock(A),
      false
  end;
lock_and_validate([A|As],Rs) ->
  lock(A),
  case lock_and_validate(As,Rs) of
    true ->
      true;
    false -> unlock(A),
      false
  end.

validate(TVar,Ver) ->
  {value,_,Act_version} = core_read(TVar),
  Ver == Act_version.

commit(WS) -> lists:map(fun({TVar,V}) -> core_write(TVar,V) end, WS).