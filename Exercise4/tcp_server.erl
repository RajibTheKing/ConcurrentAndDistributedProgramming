-module(tcp_server).
-export([start/0]).

-define(TCP_OPTS, [
    list, 
    {packet, line},
    {reuseaddr, true}
]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(65065, ?TCP_OPTS),
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
    spawn(fun() -> acceptThread(ListenSocket, self(), ServerPid) end),
    receive
        {next, Sock} ->
            ServerPid ! {new_client, Sock},
            portListener(ListenSocket, ServerPid)
    end.

acceptThread(ListenSocket, NotifyPid, ServerPid) ->
    base:printLn("Waiting for new Client to accept"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    base:printLn("New Client accepted"),
    NotifyPid ! {next, AcceptSocket},
    receiverProcess(AcceptSocket, ServerPid).

receiverProcess(Sock, ServerPid) ->
    receive
        {tcp,Sock,Str} ->
            base:printLn("Received Message: " ++ base:show(Str)),
            
            receiverProcess(Sock, ServerPid)
    end.
