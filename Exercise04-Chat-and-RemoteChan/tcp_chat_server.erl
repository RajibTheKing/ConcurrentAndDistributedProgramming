-module(tcp_chat_server).

-export([start/0]).

start() ->
  {ok, LSock} = gen_tcp:listen(65065, [list, {packet, line},{reuseaddr, true}]),
  Chat_server = spawn_link(fun() -> chat_server([]) end),
  accept_loop(LSock,Chat_server).

accept_loop(LSock,Chat_server) ->
  Me = self(),
  spawn(fun() -> request_handler(LSock,Chat_server,Me) end),
  receive
    next -> accept_loop(LSock,Chat_server)
  end.

request_handler(LSock,Chat_server,Parent) ->
  {ok, Sock} = gen_tcp:accept(LSock), %blocked
  Parent ! next,
  request_handler_loop(Sock,Chat_server).

%Receive tcp messages and forward them to the chat server
%44 = ,
%10 = \n


request_handler_loop(Sock,Chat_server) ->
  receive
    {tcp,Sock,Str} ->
      base:printLn("Exactly ---> " ++ Str),
      case lists:splitwith(fun(C) -> C/=44 end, lists:delete(10,Str)) of
        {"login",[_|Name]} ->
          Chat_server!{login,Name,Sock},
          request_handler_loop(Sock,Chat_server);
        {"message",[_|Msg]} ->
          Chat_server!{message,Sock,Msg},
          request_handler_loop(Sock,Chat_server)
      end;
    {tcp_closed, Sock} ->
      Chat_server!{logout,Sock}
  end,
  gen_tcp:close(Sock).

%Like the old chat_server, but Clients is a list of Sockets and Names
%Clients don't need to send there pids, as they are identified by there tcp connection
chat_server(Clients) ->
  receive
    {login,Name,Sock} ->
      base:printLn("login: "++ Name),
      broadcast(Clients,"login," ++ Name ++ "\n"),
      gen_tcp:send(Sock,"loggedin," ++ base:show(lists:map(fun({_,N}) -> N end, Clients)) ++ "\n"),
      chat_server([{Sock,Name}|Clients]);
    {logout,Sock} ->
      case lists:keyfind(Sock,1,Clients) of
        false -> chat_server(Clients);
        {_Sock,Name} -> NewClients = lists:keydelete(Sock,1,Clients),
          base:printLn("logout: " ++ Name),
          broadcast(NewClients,"logout," ++Name ++ "\n"),
          chat_server(NewClients)
      end;
    {message,Sock,Msg} ->
      case lists:keyfind(Sock,1,Clients) of
        false -> chat_server(Clients);
        {_Sock,Name} -> OtherClients = lists:keydelete(Sock,1,Clients),
          base:printLn("message: " ++ Name ++ "> " ++ Msg),
          broadcast(OtherClients,"message," ++ Name ++ "> " ++ Msg ++ "\n"),
          chat_server(Clients)
      end;
    Msg -> base:printLn("Unknown meesage: "++base:show(Msg)),
      chat_server(Clients)
  end.

broadcast(Clients,Msg) -> lists:map(fun({Sock,_}) -> gen_tcp:send(Sock,Msg) end, Clients).