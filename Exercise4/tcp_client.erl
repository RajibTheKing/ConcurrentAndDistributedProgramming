-module(tcp_client).
-export([join/0]).

join() ->
    case gen_tcp:connect({127,0,0,1}, 8091, [binary, {active,true}]) of
        {ok,ServerSocket} -> 
            base:printLn("Connect Successful "),
            client_input(ServerSocket),
            spawn(fun() -> client_output(ServerSocket) end);
        Other -> 
            base:printLn("Connect Failed: "++ base:show(Other))
    end.

client_input(ServerSocket) ->
    base:printLn("Enter message to send: "),
    Msg = base:getLn(),
    gen_tcp:send(ServerSocket, base:show(Msg ++ "\n")),
    client_input(ServerSocket).

client_output(ServerSocket) ->
    {ok,ServerMsg} = gen_tcp:recv(ServerSocket,0),
    base:printLn("Got Message: " ++ base:show(ServerMsg)),
    client_output(ServerSocket).
