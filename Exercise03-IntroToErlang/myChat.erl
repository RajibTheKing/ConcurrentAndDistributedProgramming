-module(myChat).
-export([start/1, join/2]).

start(Name) ->
    base:printLn(base:show(Name) ++ ": First Person who starting a New Chat Room"),
    Me = self(),
    register(server, Me),
    process_flag(trap_exit, true),
    spawn(fun() -> client_input(Name, Me) end),
    server_loop(Name, [{Name,Me}]).

join(Node, Name) ->
    Me = self(),
    register(server, Me),
    process_flag(trap_exit, true),
    {server,Node} ! {login, Name, Me},
    spawn(fun() -> client_input(Name, Me) end),
    server_loop(Name, []).


server_loop(MyName, Clients) ->
    receive
        {login,Name,CPid} ->
            base:printLn(base:show(MyName) ++ ": I have got a new Client"),
            case lookup(Name,Clients) of
                nothing  -> link(CPid),
                            NewList = [{Name, CPid}|Clients],
                            broadcast(others(NewList),{logged_in, NewList}),
                            server_loop(MyName, NewList);
                {just,_} -> CPid ! name_occupied,
                            server_loop(MyName, Clients)
            end;
        name_occupied ->
            base:printLn("Name is already in use. Please choose another name."),
            server_loop(MyName, Clients);
        
        {logged_in, UpdatedClients} ->
            base:printLn(base:show(getNames(UpdatedClients))),
            server_loop(MyName, UpdatedClients);

        {message, Text, Name} ->
            base:printLn(base:show(Name) ++ ": " ++ base:show(Text)),
            server_loop(MyName, Clients);

        {logout,Name} ->
            case lookup(Name,Clients) of
                nothing     -> server_loop(MyName, Clients);
                {just,_} -> New_clients = lists:filter(fun ({N,_}) -> Name /= N end, Clients),
                                server_loop(MyName, New_clients)
            end;
        {'EXIT',CPid,_} -> 
            case lookup2(CPid, Clients) of
                nothing -> server_loop(MyName, Clients);
                {just, Name} -> broadcast(Clients, {logout, Name}),
                                server_loop(MyName, Clients)
            end;

        {send, Msg} ->
            broadcast(others(Clients), {message, Msg, MyName}),
            server_loop(MyName, Clients);
        Msg -> 
            base:printLn("Unexpected message: " ++ base:show(Msg)),
            server_loop(MyName, Clients)
        
    end.

client_input(MyName, MyPid) ->
  base:print(base:show(MyName) ++ ": "),
  case base:getLn() of
    Text   -> MyPid ! {send,Text},
              client_input(MyName,MyPid)
  end.


% Utility functions
%%%%%%%%%%%%%%%%%%%

% Send a Msg to all Clients
broadcast(Clients, Msg) -> lists:map(fun(P)-> snd(P)!Msg end,Clients).

% Return the list of Clients with CPid removed
remove(CPid,Clients) -> lists:filter(fun(P)-> snd(P)/=CPid end, Clients).

% Return the list of Clients with ourself removed
others(Clients) -> remove(self(),Clients).

% Return the names of the Clients
getNames(Clients) -> lists:map(fun(X) -> fst(X) end,Clients).

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