-module(queueImpl).
-export([test/0, newQueue/1, stackHandler/0, moveAll/3]).
-import(testStackMailbox, [newstack/1, push/2, pop/1]).


newQueue(ToCall) ->
    Ts = spawn(fun() -> stackHandler() end),
    S1 = newstack(Ts),
    S2 = newstack(Ts), 
    Q = spawn(fun() -> queue(Ts, ToCall, S1, S2) end),
    Q.

stackHandler() ->
    receive
        {getpop, Pid, Stack} -> Stack ! {pop},
                                receive
                                    V -> Pid ! V
                                end,
                                stackHandler()
    end.

moveAll(Handler, S1, S2) ->
    Handler ! {getpop, self(), S1},
    receive
        blank -> ok;
        V -> push(S2, V), moveAll(Handler, S1, S2)
    end.


queue(Handler, Pid, Stack1, Stack2) ->
    receive
        {push, V} -> push(Stack1, V), queue(Handler, Pid, Stack1, Stack2);
        {pop} -> Handler ! {getpop, self(), Stack2},
                 receive
                     blank ->  moveAll(Handler, Stack1, Stack2), 
                               Handler ! {getpop, self(), Stack2}, 
                               receive
                                   V -> Pid ! V
                               end,
                               queue(Handler, Pid, Stack1, Stack2);
                     V -> Pid ! V, queue(Handler, Pid, Stack1, Stack2)
                end
    end.



enQueue(Queue, V) ->
    Queue ! {push, V}.

deQueue(Queue) ->
    Queue ! {pop},
    receive
        V -> V
    end.

test() ->
    Me = self(),
    Queue = newQueue(Me),
    enQueue(Queue, 10),
    enQueue(Queue, 15),
    enQueue(Queue, 20),
    enQueue(Queue, 25),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    enQueue(Queue, 30),
    enQueue(Queue, 35),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    enQueue(Queue, 50),
    enQueue(Queue, 60),
    enQueue(Queue, 70),
    enQueue(Queue, 80),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))),
    base:printLn("deQueue: " ++ base:show(deQueue(Queue))).


