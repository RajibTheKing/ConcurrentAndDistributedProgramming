-module(store).
-export([start/0]).

start() -> store_loop([]).

store_loop(DB) ->
  base:printLn(base:show(DB)),
  receive
    {store,Key,Value} -> store_loop(insert(Key,Value,DB));
    {lookup,Key,CPid} -> CPid ! lookup(Key,DB),
                         store_loop(DB)
  end.

lookup(_,[]) -> nothing;
lookup(K,[{K,V}|_]) -> {just,V};
lookup(K,[_|KVs]) -> lookup(K,KVs).

insert(K,V,KVs) -> [{K,V}|KVs].

