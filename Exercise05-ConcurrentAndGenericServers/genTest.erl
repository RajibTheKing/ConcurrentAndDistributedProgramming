-module(genTest).
-behaviour(gen_server).
-export([start/0, init/1, handle_call/3, handle_cast/2]).

start() ->
     gen_server:start_link({local, genTest}, ?MODULE, [], []),
     test().


init([]) ->
     {ok, done}.

handle_call({value, X}, _From, _State) ->
     base:printLn("Inside handle_call"),
     {reply, {okkk, X}, done}.

handle_cast(_MSg, _State) -> not_implemented.

test() ->
     base:printLn("Hello World"),
     {V, X} = gen_server:call(?MODULE, {value, 10}),
     base:printLn(base:show(V) ++ " and " ++ base:show(X)).