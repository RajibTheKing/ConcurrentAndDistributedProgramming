-module(chat_server).
-export([start/0]).
-import(lists,[map/2,filter/2]).

%Start Server with a node name
%erl.exe -sname server

start() -> register(server,self()),
           process_flag(trap_exit,true),
           server_loop([]).


server_loop(Clients) ->
  receive
    {login,Name,CPid} ->
       base:printLn("I have got a new Client"),
       case lookup_snd(Name,Clients) of
         nothing  -> link(CPid),
                     CPid ! {logged_in,map(fun ({_,Snd}) -> Snd end,Clients),self()},
                     broadcast(Clients,{new_client,Name},none),
                     server_loop([{CPid,Name}|Clients]);
         {just,_} -> CPid ! name_occupied,
                     server_loop(Clients)
       end;
    {message, Text, CPid} ->
      base:printLn("I have got a new Message"),
       case base:lookup(CPid,Clients) of
         nothing     -> server_loop(Clients);
         {just,Name} -> broadcast(Clients,{message, Text, Name}, CPid),
                        server_loop(Clients)
       end;
    {logout,CPid} ->
       case base:lookup(CPid,Clients) of
         nothing     -> server_loop(Clients);
         {just,Name} -> New_clients = filter(fun ({Pid,_}) -> CPid /= Pid end,Clients),
                        broadcast(New_clients,{logout,Name},none),
                        server_loop(New_clients)
       end;
    {'EXIT',CPid,_} -> self()!{logout,CPid},
                       server_loop(Clients);
    Msg -> base:printLn("Unexpected message: " ++ base:show(Msg)),
           server_loop(Clients)
  end.

broadcast(Clients,Msg,Sender) -> map(fun ({CPid,_}) -> case CPid==Sender of
                                                         true  -> ok;
                                                         false -> CPid ! Msg
                                                       end
                                     end,
                                     Clients).


lookup_snd(_,[])        -> nothing;
lookup_snd(K,[{V,K}|_]) -> {just,V};
lookup_snd(K,[_|KVs])   -> lookup_snd(K,KVs).

