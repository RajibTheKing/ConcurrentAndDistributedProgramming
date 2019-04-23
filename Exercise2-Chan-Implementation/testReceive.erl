-module(testReceive).
-export([startProgram/0, messageParser1/0, messageParser2/2]).

messageParser2(ProcessID, 0) ->
    ProcessID ! terminateProgram;

messageParser2(ProcessID, N) ->
    ProcessID ! {firstFunction, self() },
    receive
        secondFunction ->
            io:format("inside messageParser2~n", []),
            messageParser2(ProcessID, N-1)
    end.



messageParser1() ->
    receive 
        terminateProgram -> 
            io:format("TestProgram is finished working~n", []);
        {firstFunction, P_ID} ->
            io:format("inside messageParser1~n", []),
            P_ID ! secondFunction,
            messageParser1()
    end.



startProgram() -> 
    ProcessID = spawn(testReceive, messageParser1, []),
    spawn(testReceive, messageParser2, [ProcessID, 3]). 