-module(counterTest).
-export([clientLoop/2, counterInc/1, startProgram/0]).

clientLoop(_, 0) ->
    PIdStr = pid_to_list(self()),
    io:format("Closing Client Thread " ++ PIdStr ++ "~n", []);

clientLoop(Counter_PID, N) ->
    Counter_PID ! {incNow, self()},
    receive
        taskComplete ->
            clientLoop(Counter_PID, N-1)
    end.



counterInc(Val) ->
    receive
        {incNow, P_ID} ->
            PIdStr = pid_to_list(P_ID),
            io:format("ClientProcessID " ++ PIdStr ++": Current CountVal = " ++ integer_to_list(Val+1) ++ "~n", []),
            P_ID ! taskComplete,
            counterInc(Val+1)
    end.



startProgram() ->
    Counter_ID = spawn(counterTest, counterInc, [0]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]),
    spawn(counterTest, clientLoop, [Counter_ID, 100]).