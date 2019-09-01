-module(myQueue).
-export([start/0, test/0, queueTest/0]).

start() ->
    base:printLn("Hello World").


blank_stack(P) ->
  stack(P),
  P!blank,
  blank_stack(P).

stack(P) ->
    receive
        {push,V} -> stack(P),
                    P!V,
                    stack(P);
        {pop} -> ok
    end.


push(Stack, V) ->
    Stack!{push, V}.

pop(Stack) ->
    Stack!{pop},
    receive
        Value -> Value
    end.

queue(P) ->
    Me = self(),
    Stack1 = spawn(fun() -> blank_stack(Me) end),
    Stack2 = spawn(fun() -> blank_stack(Me) end),
    queueController(P, Stack1, Stack2).


queueController(P, S1, S2) ->
    receive
        {push, V} -> push(S1, V), queueController(P, S1, S2);
        {pop} ->    case pop(S2) of 
                       blank -> moveAllValue(S1, S2), 
                                P!pop(S2),
                                queueController(P, S1, S2);
                       Value -> P!Value, queueController(P, S1, S2)
                    end
    end.

moveAllValue(S1, S2) ->
    case pop(S1) of 
        blank -> ok ;
        Value -> push(S2, Value), moveAllValue(S1, S2)
    end.

pushQ(Queue, V) ->
    Queue ! {push, V}.

popQ(Queue) ->
    Queue!{pop},
    receive
        Value -> Value
    end.

queueTest() ->
    Me = self(),
    Queue = spawn(fun() -> queue(Me) end),
    pushQ(Queue, 10),
    pushQ(Queue, 15),
    pushQ(Queue, 20),
    pushQ(Queue, 25),
    pushQ(Queue, 30),
    pushQ(Queue, 35),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    pushQ(Queue, 45),
    pushQ(Queue, 50),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    pushQ(Queue, 55),
    pushQ(Queue, 60),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))),
    base:printLn("PopQ: " ++ base:show(popQ(Queue))).
    

test() ->
    Me = self(),
    Stack = spawn(fun() -> blank_stack(Me) end),
    push(Stack, 10),
    push(Stack, 15),
    push(Stack, 20),
    push(Stack, 25),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    push(Stack, 30),
    push(Stack, 35),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))).




