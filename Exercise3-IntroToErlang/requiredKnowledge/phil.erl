-module(phil).
-export([start/1]).

stick() -> spawn(fun() -> stick_available() end).

stick_available() ->
  receive
    {take,P} -> P ! took,
                stick_nonavailable()
  end.

stick_nonavailable() ->
  receive
    put -> stick_available()
  end.

take(Stick) ->
  Stick ! {take,self()},
  receive
    took -> took
  end.

put(Stick) ->
  Stick ! put.

start(N) -> Sticks = lists:map(fun(_) -> stick() end, lists:seq(1,N)),
            create_phils(0,Sticks),
            base:getLn(),
            spawn(fun() -> phil(N,lists:last(Sticks),hd(Sticks)) end).

create_phils(_,[_]) -> ok;
create_phils(N,[S1,S2|Sticks]) -> spawn(fun() -> phil(N,S1,S2) end),
                                  create_phils(N+1,[S2|Sticks]).

phil(N,SL,SR) ->
  base:printLn("Thinking: "++base:show(N)),
  take(SL),
  take(SR),
  base:printLn("Eating: "++base:show(N)),
  put(SL),
  put(SR),
  phil(N,SL,SR).






