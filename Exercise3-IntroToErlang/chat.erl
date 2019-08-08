% Distributed chat
-module(chat).

% User interface
-export([join/2,start/1]).

% Start a new chat with a given Name
start(Name) -> startChat([], Name).

% Join an existing chat with a given Name
join(Node,Name) -> connect({chat,Node}, Name).

% Connect to a chat via a given chat client
connect(Pid,Name) ->
  Pid!{login,self(),Name},
  receive
    {welcome,Clients} -> startChat(Clients,Name);
    nameOccupied      -> base:printLn("Name occupied. Try another name.")
  after 2000          -> base:printLn("Chat not reachable. Timeout.")
  end.

% Start the chat
startChat(OtherClients, Name) ->
  % Link to the other clients
  process_flag(trap_exit, true),
  lists:map(fun(P) -> link(fst(P)) end, OtherClients),

  % Register ourselves and introduce ourselves to the other clients
  register(chat, self()),
  broadcast({join,self(),Name},OtherClients),
  AllClients = [{self(),Name}|OtherClients],

  % Setup the shell
  base:printLn("Welcome to this wonderful chat."),
  base:print("The following people are online: "),
  base:print(getNames(AllClients)),
  base:printLn(""),
  Me = self(),
  spawn_link(fun() -> keyboard(Me,Name) end),

  % Run!
  run(AllClients),
  % finally unregister ourselves
  unregister(chat),
  bye.

% Main process of the chat
run(Clients) ->
  receive
  % Messages from other clients

    {login, CPid, Name} ->
      case lookup2(Name,Clients) of
        nothing ->
          CPid!{welcome,Clients},
          run(Clients);
        {just,_Pid} ->
          CPid!nameOccupied,
          run(Clients)
      end;

    {join,CPid,Name} ->
      base:printLn(base:show(Name) ++ " joined the chat."),
      run([{CPid,Name}|Clients]);

    {logout,CPid} ->
      case lookup(CPid, Clients) of
        {just,Name} ->
          base:printLn(base:show(Name) ++ " left the chat."),
          run(remove(CPid,Clients));
        nothing -> run(Clients)
      end;

    {msg,Name,Msg} ->
      base:printLn(base:show(Name) ++ ": " ++ Msg),
      run(Clients);

    {'EXIT',CPid,_Reason} ->
      broadcast({logout,CPid},remove(CPid,Clients)),
      run(Clients);

  % Messages from the keyboard

    quit -> broadcast({logout,self()},others(Clients));

    {sendmsg,Msg} ->
      case lookup(self(),Clients) of
        nothing     -> base:printLn("ERROR: Own name unknown.");
        {just,Name} -> broadcast({msg,Name,Msg},others(Clients))
      end,
      run(Clients);

    who ->
      base:print("The following people are online: "),
      base:print(getNames(Clients)),
      base:printLn(""),
      run(Clients)
  end.

% Keyboard process. Parses the inputs and sends a message to the client process
keyboard(CPid,Name) ->
  base:printLn("Here 1"),
  Str = base:getLn(),
  base:printLn("Here 2"),
  case Str of
    ":q"   -> CPid!quit;
    ":who" -> CPid!who, keyboard(CPid,Name);
    _      -> CPid!{sendmsg,Str}, keyboard(CPid,Name)
  end.

% Utility functions
%%%%%%%%%%%%%%%%%%%

% Send a Msg to all Clients
broadcast(Msg,Clients) -> lists:map(fun(P)-> fst(P)!Msg end,Clients).

% Return the list of Clients with CPid removed
remove(CPid,Clients) -> lists:filter(fun(P)-> fst(P)/=CPid end, Clients).

% Return the list of Clients with ourself removed
others(Clients) -> remove(self(),Clients).

% Return the names of the Clients
getNames(Clients) -> lists:map(fun(X) -> snd(X) end,Clients).

% Lookup the value for the key
lookup(_K,[]) -> nothing;
lookup(K,[{K,V}|_]) -> {just,V};
lookup(K,[_|KVs]) -> lookup(K,KVs).

% Lookup the key for a value
lookup2(_V,[]) -> nothing;
lookup2(V,[{K,V}|_]) -> {just,K};
lookup2(V,[_|KVs]) -> lookup2(V,KVs).

% First element of a pair
fst({X,_Y}) -> X.

% Second element of a pair
snd({_X,Y}) -> Y.