-module(tm_test).
-export([new_stack/0,push/2,pop/1, test/0, tm_start/0]).

%Use the implemenation shown in the lecture to implement a Turing machine for the language {anbncn where n>=0}.
%accepted string: aaabbbccc, aabbcc, abc, aaaabbbbcccc
%failed string: aabbccc, aaabbc, aabbcca


tm_start() ->
  SR = new_blank_stack(),
  Input = base:getLn(),
  lists:map(fun(Char) -> push(SR,[Char]) end,lists:reverse(Input)),
  SL = new_blank_stack(),
  Res = delta(SL,SR,q0),
  base:printLn(Res),
  output(SR).
  
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



go_left(SL,SR) -> push(SR,pop(SL)). %to go left we need to pop the next letter from the left stack and push it to the right one

delta(SL,SR,q0) -> 
  V = pop(SR),
  case V of
    blank -> push(SL,blank), delta(SL,SR,qaccept);
    "a" -> push(SL,"A"), delta(SL,SR,qb);
    _ -> delta(SL,SR,qfail)
  end;
  
delta(SL,SR,qb) ->
  V = pop(SR),
  case V of
    "a" -> push(SL,"a"), delta(SL,SR,qb);
    "B" -> push(SL,"B"), delta(SL,SR,qb);
    "b" -> push(SL,"B"), delta(SL,SR,qc);
    _ -> delta(SL,SR,qfail)
  end;
  
delta(SL,SR,qc) -> 
  V = pop(SR),
  case V of
    "b" -> push(SL,"b"), delta(SL,SR,qc);
    "C" -> push(SL,"C"), delta(SL,SR,qc);
    "c" -> push(SL,"C"), delta(SL,SR,qt1);
    _ -> delta(SL,SR,qfail)
  end;

delta(SL,SR,qt1) ->
  V = pop(SR),
  case V of
    blank -> push(SR,blank), go_left(SL,SR), delta(SL,SR,qt2);
    "c" -> push(SR,"c"), go_left(SL,SR), delta(SL,SR,q1);
    _ -> delta(SL,SR,qfail)
  end;
  
delta(SL,SR,qt2) ->
  V = pop(SR),
  case V of
    "A" -> push(SR,"A"), go_left(SL,SR), delta(SL,SR,qt2);
    "B" -> push(SR,"B"), go_left(SL,SR), delta(SL,SR,qt2);
    "C" -> push(SR,"C"), go_left(SL,SR), delta(SL,SR,qt2);
    blank -> push(SR,blank), go_left(SL,SR), delta(SL,SR,qaccept);
    _ -> delta(SL,SR,qfail)
  end;
  
delta(SL,SR,q1) -> 
  V = pop(SR),
  case V of
    "a" -> push(SR,"a"), go_left(SL,SR), delta(SL,SR,q1);
    "b" -> push(SR,"b"), go_left(SL,SR), delta(SL,SR,q1);
    "B" -> push(SR,"B"), go_left(SL,SR), delta(SL,SR,q1);
    "C" -> push(SR,"C"), go_left(SL,SR), delta(SL,SR,q1);
    "A" -> push(SL,"A"), delta(SL,SR,q0);
    _ -> delta(SL,SR,qfail)
  end;
  
delta(_,_,qaccept) -> base:printLn("accept");
delta(_,_,qfail) -> base:printLn("fail").


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