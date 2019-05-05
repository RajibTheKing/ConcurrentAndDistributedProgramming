-module(keyValueServerTCP).
-export([server/0,client/0]).

server() -> 
  {ok,LSock} = gen_tcp:listen(65002, [list, {packet,line}, {reuseaddr,true}]),
  DB = spawn(fun() -> store:start() end),
  acceptLoop(LSock,DB).

acceptLoop(LSock,DB) ->
  Me = self(),
  spawn(fun() -> requestHandler(LSock, DB, Me) end),
  receive
    next -> acceptLoop(LSock,DB)
  end.

requestHandler(LSock,DB,Parent) ->
  {ok,Sock} = gen_tcp:accept(LSock),
  Parent ! next,
  receive
    {tcp,Sock,Str} ->
       case lists:splitwith(fun(C) -> C/= 44 end,
                            lists:delete(10,Str)) of
         {"store",[_|Args]} ->
             {Key,[_|Value]} = lists:splitwith(fun(C) -> C/= 44 end,Args),
             DB ! {store,Key,Value};

         {"lookup",[_|Key]} ->
             DB ! {lookup,Key,self()},
             receive
               {just,Ans} -> gen_tcp:send(Sock,"{just," ++ Ans ++ "}\n");
               noting     -> gen_tcp:send(Sock,"nothing\n")
             end
       end
  end,
  gen_tcp:close(Sock).      



client() ->
  case base:getLn("(s)tore, (l)ookup: ") of
    "s" -> Key   = base:getLn("Key:   "),
           Value = base:getLn("Value: "),
           {ok,Sock} = gen_tcp:connect("localhost",65002,[list, {packet,line}, 
                                                          {active, false}]),
           gen_tcp:send(Sock,"store," ++ Key ++ "," ++ Value ++ "\n"),
           gen_tcp:close(Sock);
   
    "l" -> Key   = base:getLn("Key:   "),
           {ok,Sock} = gen_tcp:connect("localhost",65002,[list, {packet,line}, 
                                                          {active, false}]),
           gen_tcp:send(Sock,"lookup," ++ Key ++ "\n"),
           case gen_tcp:recv(Sock,0) of
             {ok,Res} -> base:printLn(Res);
             Other    -> base:printLn("Other value: " ++ base:show(Other))
           end,
           gen_tcp:close(Sock)
  end.






