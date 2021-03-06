-module(tm).
-export([new_stack/0,push/2,pop/1, test/0, tm_start/0]).

tm_start() ->
  SR = new_blank_stack(),
  Input = base:getLn(),
  lists:map(fun(Char) -> push(SR,[Char]) end,lists:reverse(Input)),
  SL = new_blank_stack(),
  Res = delta(SL,SR,q0),
  base:printLn(Res),
  output(SR).

delta(SL,SR,q0) ->
  A = pop(SR),
  case A of
    "a" -> push(SL,"a"),
           delta(SL,SR,q0);
    "b" -> push(SL,"b"),
           delta(SL,SR,q1);
    blank -> push(SR,blank),
             delta(SL,SR,f)
  end;

delta(SL,SR,q1) ->
  A = pop(SR),
  case A of
    "a" -> push(SL,"a"),
           delta(SL,SR,r);
    "b" -> push(SL,"b"),
           delta(SL,SR,q1);
    blank -> push(SR,blank),
             delta(SL,SR,f)
  end;

delta(SL,SR,r) ->
  failure;

delta(SL,SR,f) ->
  accept.

output(S) ->
  case pop(S) of
    blank  -> base:printLn("");
    Letter -> base:print(Letter),
              output(S)
  end. 

new_blank_stack() -> Me = self(),
                     spawn(fun() -> blank_stack(Me) end).

blank_stack(P) ->
  stack(P),
  P!blank,
  blank_stack(P).

new_stack() -> Me = self(),
               spawn(fun() -> stack(Me) end).

stack(P) ->
  receive
    {push,V} -> stack(P),
                P!V,
                stack(P);
    pop      -> ok
  end.

push(Stack,V) ->
  Stack!{push,V}.

pop(Stack) ->
  Stack!pop,
  receive
    Ans -> Ans
  end.

test() ->
  S = new_blank_stack(),
  push(S,42),
  push(S,73),
  base:printLn(pop(S)),
  push(S,7),
  push(S,8),
  base:printLn(pop(S)),
  base:printLn(pop(S)),
  base:printLn(pop(S)),
  base:printLn(pop(S)),
  push(S,7),
  push(S,8),
  base:printLn(pop(S)).

