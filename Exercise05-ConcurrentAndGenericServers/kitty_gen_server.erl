-module(kitty_gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).


start_link() -> 
    {ok, P} = gen_server:start_link(?MODULE, ["Rajib", "Sazib"], []),
    gen_server:call(P, check).

init(Param) ->
    base:printLn("TheKingLog --> Here"),
    base:printLn("TheKingLog--> " ++ base:show(Param)),
    {ok, self()}.

handle_call(check, _From, _State) ->
    base:printLn("Inside handle_call").

handle_cast(_Msg, _State) -> 
    not_implemented.


