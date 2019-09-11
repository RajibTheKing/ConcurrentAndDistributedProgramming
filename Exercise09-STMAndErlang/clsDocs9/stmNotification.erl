-module(stmNotification).
-export([new_tvar/1,write_tvar/2,read_tvar/1,
         atomically/1,retry/0,or_else/2,
         test/0,testLoop/0]).
-import(gb_trees,[empty/0,insert/3,enter/3,lookup/2,keys/1,to_list/1]).
-import(lists,[map/2]).

new_tvar(V) -> spawn(fun() -> tvar(V,[]) end).

tvar(V,Susps) ->          
  receive
    {read,P,Susp} -> P ! {value,V},
                     tvar(V,add(Susp,Susps));
    {lock,P}      -> P ! locked,
                     tvar_locked(V,Susps);
    {unsusp,P}    -> tvar(V,Susps--[P])
  end.

tvar_locked(V,Susps) ->
  receive
    {write,V1} -> map(fun(P) -> P!modified end, Susps),
                  tvar_locked(V1,[]);
    {unsusp,P} -> tvar_locked(V,Susps--[P]);
    unlock     -> tvar(V,Susps)
  end.

add(X,[])     -> [X];
add(X,[X|Ys]) -> [X|Ys];
add(X,[Y|Ys]) -> [Y|add(X,Ys)].

core_read(T,P) -> T ! {read,self(),P},
                  receive
                    {value,V} -> V
                  end.

core_write(T,V) -> T ! {write,V}.

lock(T,P) -> T!{lock,P},
             receive
                locked  -> ok
             end.

unlock(T) -> T!unlock.

unsusp(Ts,P) -> map(fun(T) -> T!{unsusp,P} end,Ts).

read_tvar(T) ->
  receive modified -> throw(rollback) after 0 -> ok end,
  {RS,WS,MI} = get(state),
  case lookup(T,WS) of
    none      -> V = core_read(T,MI),
                 RS1 = gb_sets:add(T,RS),
                 put(state,{RS1,WS,MI}),
                 receive modified -> throw(rollback) after 0 -> ok end,
                 V;
    {value,V} -> V
  end.

write_tvar(T,V) ->
  receive
   modified -> throw(rollback)
  after 0 -> ok
  end,
  {RS,WS,MI} = get(state),
  put(state,{RS,enter(T,V,WS),MI}).

retry() -> throw(retry).

or_else(Trans1,Trans2) -> 
  {_,WS1,_} = get(state),
  case catch Trans1() of
    rollback -> throw(rollback);
    retry    -> {RS2,_,P} = get(state),
                put(state,{RS2,WS1,P}),
                Trans2();
    Res      -> Res
  end.

oneRestartOnly(P) -> receive
                       modified -> P!modified,
                                   noRestart(P);
                       locked  -> P!locked,
                                  oneRestartOnly(P);
                       stop    -> ok
                     end.
noRestart(P) -> receive
                  locked  -> P!locked,
                             noRestart(P);
                  stop    -> ok
                end.

atomically(Trans) ->
  Me = self(),
  MI = spawn(fun() -> oneRestartOnly(Me) end),
  put(state,{gb_sets:empty(),empty(),MI}),
  case catch Trans() of
    rollback -> {RS,_,_} = get(state),
                unsusp(gb_sets:to_list(RS),MI),
                MI!stop,
                atomically(Trans);
    retry -> {RS,_,_} = get(state), 
             RL = gb_sets:to_list(RS),
             receive
               modified -> unsusp(RL,MI),
                           MI!stop,
                           atomically(Trans)
             end;
    Res -> {RS,WS,_} = get(state),
           RL = gb_sets:to_list(RS),
           Tvars = lists:umerge(RL,keys(WS)),
           map(fun(Tvar) -> lock(Tvar,MI) end, Tvars),
           receive
             modified -> unsusp(RL,MI),
                         map(fun(Tvar) -> unlock(Tvar) end, Tvars),
                         MI!stop,
                         atomically(Trans)
           after 0 -> map(fun({Tvar,V}) -> core_write(Tvar,V) end,
                          to_list(WS)),
                      unsusp(RL,MI),
                      map(fun(Tvar) -> unlock(Tvar) end, Tvars),
                      MI!stop,
                      Res
           end
  end.

test() -> T1 = atomically(fun() -> new_tvar(42) end),
          T2 = atomically(fun() -> new_tvar(73) end),
          spawn(fun() -> test1(T1,3) end),
          test2(T1,T2,3),
          base:print("test2 ist fertig "),
          base:getLn(),
          V1 = atomically(fun() -> read_tvar(T1) end),
          V2 = atomically(fun() -> read_tvar(T2) end),
          base:print({V1,V2}).

test1(_,0) -> base:print("test1 ist fertig");
test1(T,N) -> atomically(fun() -> V1 = read_tvar(T),
                                  write_tvar(T,V1+1)
                         end),
              test1(T,N-1).
          
test2(_, _, 0) -> ok;
test2(T1,T2,N) ->
          atomically(fun() -> V1 = read_tvar(T1),
                              V2 = read_tvar(T2),
                              write_tvar(T1,V1+V2)
                     end),
          test2(T1,T2,N-1).

testLoop() ->
  T1 = atomically(fun() -> new_tvar(42) end),
  T2 = atomically(fun() -> new_tvar(42) end),
  spawn(fun() -> dec(T1,T2,1000) end),
  spawn(fun() -> dup(T1,T2,1000) end),
  check(T1,T2,2000).

check(_,_,0) -> ok;
check(T1,T2,N) -> atomically(fun() -> V1 = read_tvar(T1),
                                      V2 = read_tvar(T2),
                                      case V1/=V2 of
                                        true -> loop();
                                        false -> ok
                                      end
                             end),
                  check(T1,T2,N-1).

loop() -> base:printLn("LOOP"), loop().

dec(_ ,_ ,0) -> ok;
dec(T1,T2,N) -> atomically (fun() -> dec(T1), dec(T2) end),
                dec(T1,T2,N-1).

dec(T) -> V1 = read_tvar(T),
          write_tvar(T,V1-1).

dup(_ ,_ ,0) -> ok;
dup(T1,T2,N) -> atomically (fun() -> dup(T1), dup(T2) end),
                dup(T1,T2,N-1).

dup(T) -> V1 = read_tvar(T),
          write_tvar(T,V1*2).







