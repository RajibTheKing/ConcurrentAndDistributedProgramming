-module(factorial).
-export([fac/1, fac2/1, len/1, app/2]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

fac2(N) -> case N of
             0 -> 1;
             _ -> Nm1 = N - 1,
                  N * fac2(Nm1)
           end.

len([])     -> 0;
len([_|Xs]) -> 1 + len(Xs).

app([],    Ys) -> Ys;
app([X|Xs],Ys) -> [X|app(Xs,Ys)].

lookup(_,[]) -> nothing;
lookup(K,[{K,V}|_]) -> {just,V};
lookup(K,[_|KVs]) -> lookup(K,KVs).

insert(K,V,KVs) -> [{K,V}|KVs].

