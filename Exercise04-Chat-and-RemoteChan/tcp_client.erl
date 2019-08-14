-module(tcp_client).
-export([join/0]).

join(Name) ->
    case gen_tcp:connect({127,0,0,1},65065,[list,{packet,line}]) of
        {ok,ServerSocket} -> 
            base:printLn("Connect Successful "),
            gen_tcp:send(ServerSocket, "login," ++ base:show(Name) ++ "\n"),
            spawn(fun() -> client_output(ServerSocket) end),
            client_input(ServerSocket);
        Other -> 
            base:printLn("Connect Failed: "++ base:show(Other))
    end.

client_input(ServerSocket) ->
    base:printLn("Enter message to send: "),
    Msg = base:getLn(),
    gen_tcp:send(ServerSocket,"message," ++ Msg ++ "\n"),
    client_input(ServerSocket).

client_output(ServerSocket) ->
    receive
        {tcp,ServerSocket,Str} ->
            base:printLn("Received Message: " ++ base:show(Str)),
            client_output(ServerSocket)
    end.