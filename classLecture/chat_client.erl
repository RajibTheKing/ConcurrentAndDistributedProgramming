-module(chat_client).
-export([join/2]).

join(Node, Name) ->
  {server,Node} ! {login,Name,self()},
  receive
    {logged_in,Names,Server_pid} ->
        base:printLn(Names),
        Me = self(),
        spawn(fun() -> client_input(Name,Server_pid,Me) end),
        client_output();
    name_occupied ->
        base:printLn("Name is already in use. Please choose another name.")
  after 2000 ->
    base:printLn("Server not reachable.")
  end.

client_input(Name,Server_pid,CPid) ->
  base:printLn(Name),
  case base:getLn() of
    "bye." -> Server_pid ! {logout,CPid},
              CPid ! logout;
    Text   -> Server_pid ! {message,Text,CPid},
              client_input(Name,Server_pid,CPid)
  end.

client_output() ->
  receive
    {message,Text,Name} ->
       base:printLn(base:show(Name) ++ "> " ++ base:show(Text)),
       client_output();
    {new_client,Name} ->
       base:printLn("joined the chat."),
       client_output();
    {logout,Name} ->
       base:printLn("left the chat."),
       client_output();
    logout -> 
       ok
  end.
