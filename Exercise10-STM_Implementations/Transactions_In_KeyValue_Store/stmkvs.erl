-module(stmkvs).

-import(gb_trees,[empty/0,enter/3,to_list/1]).

-export([new_key_store/0, insert/2, lookup/1, delete/1, atomically/2]).

version_supply(N) ->
  receive
    {next, Pid} -> Pid ! {version, N}, version_supply(N + 1)
  end.

next_version() ->
  versions ! {next, self()},
  receive
    {version, N} -> N
  end.

new_key_store() ->
  VS = spawn(fun() -> version_supply(0) end),
  case catch register(versions, VS) of
    true -> ok;
    _    -> unregister(versions), register(versions, VS)
  end,
  spawn(fun () -> key_store(empty()) end).

key_store(Kvs) ->
  receive
    {lookup, Key, Pid} ->
      Result = case gb_trees:lookup(Key, Kvs) of
                 {value, {{just, V}, Version, _Susps}} -> {value, {V, Version}};
                 _                                     -> none
               end,
      Pid ! Result,
      key_store(Kvs);
    {lock, Pid} ->
      Pid ! locked,
      key_store_locked(Kvs)
  end.

key_store_locked(Kvs) ->
  receive
    {lookup, Key, Pid}        ->
      Result = case gb_trees:lookup(Key, Kvs) of
                 {value, {{just, V}, Version, _Susps}} -> {value, {V, Version}};
                 _                                     -> none
               end,
      Pid ! Result,
      key_store_locked(Kvs);
    {write, WriteList}        ->
      key_store_locked(lists:foldl(fun core_write/2, Kvs, WriteList));
    {validate, Versions, Pid} -> Pid ! {valid, validate(Versions, Kvs)}, key_store_locked(Kvs);
    {sleeping, Key, Pid} -> case gb_trees:lookup(Key, Kvs) of
                              none -> V = next_version(),
                                Kvsnew = enter(Key, {nothing, V, [Pid]}, Kvs),
                                key_store_locked(Kvsnew);
                              {value, {Val, Ver, Susps}} -> Kvsnew = enter(Key, {Val, Ver, [Pid |Susps]}, Kvs),
                                key_store_locked(Kvsnew)
                            end;
    unlock -> key_store(Kvs)
  end.

core_lookup(Key, Store) ->
  Store ! {lookup, Key, self()},
  receive
    none            -> throw(retry);
    {value, ValVer} -> ValVer
  end.

core_write({Key, delete}, Kvs) ->
  case gb_trees:lookup(Key, Kvs) of
    none -> Kvs;
    {value, {_Value, Version, _Susps}} ->
      enter(Key, {nothing, Version}, Kvs)
  end;
core_write({Key, {insert, Value}}, Kvs) ->
  Version = next_version(),
  case gb_trees:lookup(Key,Kvs) of
    none -> okay;
    {value,{_, _, Susps}} -> lists:map(fun(P) -> P ! continue end, Susps)
  end,
  enter(Key, {{just, Value}, Version,[]}, Kvs).

sleeping(T) -> T!{sleeping,self()}.

validate([]                       , _Kvs) -> true;
validate([{Key,Version}|KVersions],  Kvs) ->
  case gb_trees:lookup(Key, Kvs) of
    {value, {{just, _V}, Version, _Susps}} -> validate(KVersions, Kvs);
    _                                      -> false
  end.

valid(Store, Rs) -> Store ! {validate, Rs, self()},
  receive {valid, X} -> X end.

insert(Key, Value) ->
  {Store, Rs, Ws} = get(state),
  NewWs           = enter(Key, {insert, Value}, Ws),
  put(state, {Store, Rs, NewWs}).

delete(Key) ->
  {Store, Rs, Ws} = get(state),
  NewWs           = enter(Key, delete, Ws),
  put(state, {Store, Rs, NewWs}).

lookup(Key) ->
  {Store, Rs, Ws} = get(state),
  {NewRs, Value} = case gb_trees:lookup(Key, Ws) of
                     % No one has written the value, it should be in the store
                     none ->
                       {Val, Ver} = core_lookup(Key, Store), % kann misslingen!
                       % Lookup if someone reads the value
                       case gb_trees:lookup(Key, Rs) of
                         % if not, everything is fine
                         none         -> {enter(Key, Ver, Rs), Val};
                         % if there is a value with the same  version, everything is fine
                         {value, Ver} -> {Rs, Val};
                         % rollback otherwise
                         {value, _}   -> throw(rollback)
                       end;
                     % someone writes the value, the new value must be used
                     {value, {insert, Val}} -> {Rs, Val};
                     % Someone deletes the value, retry.
                     {value, delete} -> throw(retry)
                   end,
  put(state, {Store, NewRs, Ws}),
  Value.

atomically(Store, Transaction) ->
  put(state, {Store, empty(), empty()}),
  case catch(Transaction()) of
    rollback -> atomically(Store, Transaction);
    retry    -> {_, Rs, _Ws} = get(state),
      lock(Store),
      case valid(Store, to_list(Rs)) of
        false -> unlock(Store), atomically(Store, Transaction);
        true  -> Me = self(),
          spawn(fun() ->
            gb_trees:map(fun(T,_) -> sleeping(T) end, Rs),
            receive
              continue -> Me!continue
            end
                end),
          receive
            continue -> atomically(Store, Transaction)
          end end;
    Result   -> {_, Rs, Ws} = get(state),
      lock(Store),
      case valid(Store, to_list(Rs)) of
        false -> unlock(Store),
          atomically(Store, Transaction);
        true  -> Store ! {write, to_list(Ws)},
          unlock(Store),
          Result
      end
  end.

lock(Store) ->
  Store ! {lock, self()},
  receive locked -> ok end.

unlock(Store) ->
  Store ! unlock.