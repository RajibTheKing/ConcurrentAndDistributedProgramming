-module(testStackMailbox).
-export([newstack/1, test/0, push/2, pop/1]).

newstack(ToCall) ->
    S = spawn(fun() -> wrapper_Stack(ToCall) end),
    S ! {pushedValue, blank},
    S.

wrapper_Stack(Pid) ->
    receive
        {push, V} -> self() ! {pushedValue , V}, wrapper_Stack(Pid);
        {pop} -> case getTop(top) of
                    blank -> self() ! {pushedValue, blank}, Pid ! blank, wrapper_Stack(Pid);
                    V -> Pid ! V, wrapper_Stack(Pid)
                 end
    end.

getTop(Last) ->
    receive
        {pushedValue, top} -> Last;
        {pushedValue, V} -> self()! {pushedValue, Last}, getTop(V)
    end.

push(Stack, V) ->
    Stack ! {push, V}.

pop(Stack) ->
    Stack ! {pop},
    receive
        Value -> Value
    end.

test() ->
    Me = self(),
    Stack = newstack(Me),
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
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    push(Stack, 50),
    push(Stack, 60),
    push(Stack, 70),
    push(Stack, 80),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))),
    base:printLn("Pop: " ++ base:show(pop(Stack))).
