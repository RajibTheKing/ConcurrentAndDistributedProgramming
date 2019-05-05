-module(tcp_server).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8091, [{active,true}, binary]),
    spawn(fun() -> portListener(ListenSocket, self()) end),
    serverloop([]).
    

serverloop(SocketList) ->
    receive
        {new_client, NewSock} ->
            serverloop([NewSock | SocketList]);
        {client_message, Msg} ->
            base:printLn("Message: " ++ base:show(Msg)),
            serverloop(SocketList)
    end.


portListener(ListenSocket, ServerPid) ->
    base:printLn("Waiting for new Client to accept"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    base:printLn("New Client accepted"),
    ServerPid ! {new_client, AcceptSocket},
    spawn(fun() -> receiverProcess(AcceptSocket, ServerPid) end),
    portListener(ListenSocket, ServerPid).


receiverProcess(Sock, ServerPid) ->
    {ok,ClientMsg} = gen_tcp:recv(Sock,0),
    ServerPid ! {client_message, ClientMsg},
    receiverProcess(Sock, ServerPid).
