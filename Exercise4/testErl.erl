-module(testErl).
-export([startingPoing/0]).

startingPoing() ->
    case base:getLn() of
        "bye." -> ok;
        _   -> rand:seed(exs1024s),
                  RNum = floor(rand:uniform() * 1000) rem 100,
                  base:printLn(base:show(RNum)),
                  startingPoing()
    end.