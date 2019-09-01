-module(queue).
-export([empty_queue/0,dequeue/1,enqueue/2]).

empty_queue() -> {[],[]}.

enqueue(V,{[],[]}) -> {[V],[]};
enqueue(V,{L1,L2}) -> {L1,[V|L2]}.

dequeue({[V|L1],L2}) -> case L1 of
                          [] -> {V,{lists:reverse(L2),[]}};
                          _  -> {V,{L1,L2}}
                        end.

