-module(mailboxStack).
-export([test/0, wrapperFunc/2]).

stack(P) -> Me = self(), wrapperFunc(P, Me).

wrapperFunc(P, Stack) ->
    receive
        {push, Value} -> Stack ! {pushedValue, Value}, wrapperFunc(P, Stack);
        {pop}         -> Stack ! {pushedValue, top}, P!getLast(Stack, blank), wrapperFunc(P, Stack)
    end.
    
getLast(MailBox, LastValue) ->
    receive
        {pushedValue, top} -> LastValue;
        {pushedValue, V} -> case LastValue of
                                blank -> getLast(MailBox, V);
                                _     -> MailBox !{pushedValue, LastValue}, getLast(MailBox, V)
                            end
    end.


push(Stack, Value) ->
    Stack ! {push, Value}.

pop(Stack) ->
    Stack ! {pop},
    receive
        Value -> Value
    end.

test() ->
    Me = self(),
    Stack = spawn(fun() -> stack(Me) end),
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
    base:printLn("Pop: " ++ base:show(pop(Stack))).