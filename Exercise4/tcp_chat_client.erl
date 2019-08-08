-module(tcp_chat_client).

-export([join/1]).

join(Name) ->
  {ok,Sock} = gen_tcp:connect({127,0,0,1},65065,[list,{packet,line}]),
  gen_tcp:send(Sock,"login," ++ Name ++ "\n"),
  Chat_client = spawn_link(fun() -> chat_client(Sock,Name) end),
  request_handler_loop(Sock,Chat_client).

%Receive tcp messages and forward them to the chat client
request_handler_loop(Sock,Chat_client) ->
  receive
    {tcp,Sock,Str} ->
      case lists:splitwith(fun(C) -> C/=44 end, lists:delete(10,Str)) of
        {"loggedin",[_|Names]} ->
          Chat_client!{loggedin,Names},
          request_handler_loop(Sock,Chat_client);
        {"login",[_|Name]} ->
          Chat_client!{login,Name},
          request_handler_loop(Sock,Chat_client);
        {"logout",[_|Name]} ->
          Chat_client!{logout,Name},
          request_handler_loop(Sock,Chat_client);
        {"message",[_|Message]} ->
          Chat_client!{message,Message},
          request_handler_loop(Sock,Chat_client)
      end
  end.

chat_client(Sock,Name) ->
  receive
    {loggedin,Other_users} ->
      base:printLn(Other_users),
      spawn_link(fun() -> keyboard(Name, Sock) end),
      client_loop()
  after
    2000 -> base:printLn("No contact to server possible.")
  end.

client_loop() ->
  receive
    {login,Name} -> base:printLn(Name++" joined."),
      client_loop();
    {logout,Name} -> base:printLn(Name++" left."),
      client_loop();
    {message,Msg} -> base:printLn(Msg),
      client_loop()
  end.

keyboard(Name, Sock) ->
  Input = base:getLn(Name++": "),
  case Input of
    %Close tcp connection to logout
    "bye." -> gen_tcp:close(Sock),exit(-1);
    _     -> gen_tcp:send(Sock,"message," ++ Input ++ "\n"),
      keyboard(Name, Sock)
  end.