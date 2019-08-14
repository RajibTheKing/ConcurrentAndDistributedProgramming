-module(game_client).
-export([join/2, client_input/3]).

join(Node, Name) ->
  {server,Node} ! {login,Name,self()},
  receive
    {logged_in,Names,Server_pid} ->
        base:printLn(Names),
        Me = self(),
        InputPid = spawn(fun() -> client_input(Name,Server_pid,Me) end),
        GuiPid = spawn(fun() -> game_gui:counterWindow(InputPid,400,400, Name) end),
        process_flag(trap_exit, true),
        client_output(GuiPid);
    name_occupied ->
        base:printLn("Name is already in use. Please choose another name.")
  after 2000 ->
    base:printLn("Server not reachable.")
  end.

client_input(Name,Server_pid,CPid) ->
  receive
    {clicked, ID} ->
        base:printLn("Button " ++ base:show(ID) ++ " clicked notified"),
        Server_pid ! {button_clicked, CPid},
        client_input(Name, Server_pid, CPid);
    {close} ->
        ok
  end.

client_output(MyGui) ->
  receive
    {button_position, X, Y} ->
       base:printLn("New Position: " ++ base:show(X) ++ ", " ++ base:show(X)),
       MyGui ! {button_create,100,X,Y},
      client_output(MyGui);
    {message,Text,Name} ->
       base:printLn(base:show(Name) ++ "> TheKing Message: " ++ base:show(Text)),
       MyGui ! {text_update, Text},
       client_output(MyGui);
    {new_client,Name} ->
       base:printLn(base:show(Name) ++ "joined the game."),
       MyGui ! {text_update, "joined the game"},
       client_output(MyGui);
    {logout,Name} ->
       base:printLn(base:show(Name) ++ "left the game."),
       client_output(MyGui);
    logout -> 
       ok
  end.
