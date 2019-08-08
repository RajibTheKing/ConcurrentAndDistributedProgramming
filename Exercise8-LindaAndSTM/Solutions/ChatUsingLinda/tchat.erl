% Tuple space chat
-module(tchat).

% User interface
-export([init_chat/0,start/2]).

% Initialize the chat server. Invoke only once and prior to any chat client
init_chat() ->
  S = linda:start(),
  register(linda,S),
  linda:out(S,{chat,S}),
  set_client_list(S,[]),
  linda:out(S,{messageid,0}),
  ok.

% Start a new chat with a given Name
start(Name,Node) ->
  % Retrieve tuple space and client list
  base:printLn("started"),
  Ts = linda:rd({linda,Node},fun({chat,P}) -> P end),
  base:printLn("Got tuple server"),
  Clients = get_client_list(Ts),
  base:printLn("Got client list"),

  case lists:member(Name,Clients) of
    true ->
      set_client_list(Ts,Clients),
      base:printLn("Name occupied. Try another name.");
    false  ->
      AllClients = insert(Name,Clients),
      set_client_list(Ts,AllClients),
      write_message(Ts,{join,Name}),
      base:print("The following people are online: "),base:print(AllClients),
      spawn(fun() -> message_handler(Ts,Name,read_message_id(Ts)) end),
      keyboard(Ts,Name),
      ok
  end.

% Message Handling
message_handler(Ts,Name,MessageId) ->
  case get_message_by_id(Ts,MessageId) of
    {join,Other}    ->
      base:printLn(base:show(Other) ++ " joined the chat."),
      message_handler(Ts,Name,MessageId+1);
    {logout,Other}  ->
      case Name == Other of
        true  ->  base:printLn("Bye.");
        false ->  base:printLn(base:show(Other) ++ " left the chat."),
          message_handler(Ts,Name,MessageId+1)
      end;
    {msg,Other,Msg} ->
      case Name == Other of
        true  -> ok;
        false -> base:printLn(base:show(Other) ++ ": " ++ Msg)
      end,
      message_handler(Ts,Name,MessageId+1)
  end.

% Keyboard process
keyboard(Ts,Name) ->
  Str = base:getLn(base:show(Name) ++ "> "),
  case Str of
    ":q"   ->
      write_message(Ts,{logout,Name}),
      set_client_list(Ts,remove(Name,get_client_list(Ts)));
    ":who" ->
      Clients = read_client_list(Ts),
      base:print("The following people are online: "),
      base:print(Clients),
      keyboard(Ts,Name);
    _      ->
      write_message(Ts,{msg,Name,Str}),
      keyboard(Ts,Name)
  end.

% Access functions for the list of clients in the chat
get_client_list(Ts)    -> linda:in(Ts, fun ({clientlist,CL}) -> CL end).
set_client_list(Ts,CL) -> linda:out(Ts,{clientlist,CL}).
read_client_list(Ts)   -> linda:rd(Ts, fun ({clientlist,CL}) -> CL end).

% Access functions for the unique message id
get_and_inc_message_id(Ts) ->
  ID = linda:in(Ts, fun ({messageid,MID}) -> MID end),
  linda:out(Ts,{messageid,ID+1}),
  ID.

read_message_id(Ts) -> linda:rd(Ts,fun ({messageid,MID}) -> MID end).

% Reading and writing messages from/to the tuple space

write_message(Ts,Message) ->
  linda:out(Ts,{msg,get_and_inc_message_id(Ts),Message},msg_timeout()).

get_message_by_id(Ts,MessageId) ->
  linda:rd(Ts,fun ({msg,MID,Msg}) when MID == MessageId -> Msg end).

write_message2(Ts,Message) ->
  base:print(length(read_client_list(Ts))),
  linda:out(Ts,{msg,get_and_inc_message_id(Ts),length(read_client_list(Ts)),Message},msg_timeout()).

get_message_by_id2(Ts,MessageId) ->
  {RL,Msg} = linda:in(Ts,
    fun ({msg,MID,ReadersLeft,Msg}) when MID == MessageId -> {ReadersLeft,Msg} end),
  case RL of
    0 -> ok;
    1 -> ok;
    _ -> linda:out(Ts,{msg,MessageId,RL-1,Msg},msg_timeout())
  end,
  Msg.

% Timeout for messages as a simple garbage collection
msg_timeout() -> 3000.

% Remove an element from a list
remove(X,XS) -> lists:filter(fun(Y)-> X/=Y end, XS).

% Insert into a sorted list, no duplicates
insert(X,[]) -> [X];
insert(X,[X|XS]) -> [X|XS];
insert(X,[Y|XS]) when X < Y -> [X|[Y|XS]];
insert(X,[Y|XS]) when X > Y -> [Y|insert(X,XS)].