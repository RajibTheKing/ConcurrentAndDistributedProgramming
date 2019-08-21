-module(remote_chan).
-export([new_chan/0,chan_register/3,chan_lookup/2,
         read_chan/1,write_chan/2,link_chan/2]).

new_chan() ->
    {ok,LSock} = gen_tcp:listen(0, [list, {packet,line}, {reuseaddr,true}]),
    {ok,Port} = inet:port(LSock),
    Chan = {local_chan,spawn(fun() -> chan() end),net_adm:localhost(),Port},
    spawn(fun() -> portListener(LSock,Chan) end),
    Chan.

portListener(LSock,Chan) ->
  case gen_tcp:accept(LSock) of
    {ok,Sock} ->
      receive
        {tcp,Sock,MsgStr} ->
           case deserialize(MsgStr) of
             {remote_msg,Msg} ->
               write_chan(Chan,Msg),
               gen_tcp:close(Sock);
               
             %ping message of another Chan (don't need to answer, as tcp connection will fail if not reachable)
             ping -> base:printLn("got ping");
             
             %if another Chan links this chan, we need to link back (see specification)
             {link,Remote_chan} ->
               link_chan(Chan,Remote_chan,false),
               gen_tcp:close(Sock)
          end, portListener(LSock,Chan)
      end
  end.

chan_register(Host,Name,Chan) ->
  case gen_tcp:connect(Host,65002,[list,{packet,line},{active,false}]) of
    {ok,Sock} -> gen_tcp:send(Sock,"store," ++ Name ++ "," ++
                                   serialize(Chan) ++ "\n");
    Other -> base:printLn("chanregister errror: "++ base:show(Other))
  end.

chan_lookup(Host,Name) ->
  case gen_tcp:connect(Host,65002,[list,{packet,line},{active,false}]) of
    {ok,Sock} -> gen_tcp:send(Sock,"lookup," ++ Name ++ "\n"),
                 case gen_tcp:recv(Sock,0) of
                   {ok,Maybe_Remote_chanStr} -> 
                     gen_tcp:close(Sock),
                     Res = deserialize(Maybe_Remote_chanStr),
                     Res
                 end;      
    Other -> base:printLn("chanregister error: "++ base:show(Other))
  end.

  
chan() ->
  receive
    {read,P} -> receive
                  {write,Msg} -> P! {chanMsg,Msg},
                                 chan();
                  exit -> P!exit
                end;
    exit -> exit
  end.

%User interface to link Chans, expecting a local and a remote chan
link_chan(Local,Remote) -> link_chan(Local,Remote,true).

%Send the remote Chan a message first, that it needs to link back
link_chan({local_chan,LPid,LHost,LPort},{remote_chan,Host,Port},true) ->
  case gen_tcp:connect(Host,Port,[list,{packet,line},{active,false}]) of
    {ok,Sock} -> 
      gen_tcp:send(Sock,serialize({link,{local_chan,LPid,LHost,LPort}})++"\n"),
      gen_tcp:close(Sock)
  end,
  spawn(fun() -> link_check(LPid,Host,Port) end);
%If this is already the other direction, do not link back
link_chan({local_chan,LPid,_,_},{remote_chan,Host,Port},false) ->
  spawn(fun() -> link_check(LPid,Host,Port) end).

%Check connection to remote Chan all 2 seconds
link_check(LPid,Host,Port) ->
  case gen_tcp:connect(Host,Port,[list,{packet,line},{active,false}]) of
    {ok,Sock} -> base:printLn("send ping"),
                 gen_tcp:send(Sock,"ping\n"),
                 gen_tcp:close(Sock);
    %If the connection gets lost, send a message to the local linked Chan and exit
    _ -> base:printLn("connection lost"),LPid!exit,exit(link_broken)
  end,
  receive
    after 2000 -> link_check(LPid,Host,Port)
  end.

read_chan({local_chan,P,_,_}) ->
  P!{read,self()},
  receive
    {chanMsg,Msg} -> Msg;
    exit -> exit
  end.

write_chan({local_chan,P,_,_},Msg) ->
  P!{write,Msg};
write_chan({remote_chan,Host,Port},Msg) ->

  case gen_tcp:connect(Host,Port,[list,{packet,line},{active,false}]) of
    {ok,Sock} ->
      gen_tcp:send(Sock,serialize({remote_msg,Msg})++"\n"),
      gen_tcp:close(Sock)
  end.

serialize({local_chan,_,Host,Port}) -> serialize({remote_chan,Host,Port});
serialize(X) when is_atom(X) -> atom_to_list(X);
serialize(X) when is_integer(X) -> integer_to_list(X);
%serialize(X) when is_pid(X) -> pid_to_list(X);
serialize(X) when is_tuple(X) -> "{"++serializeList(tuple_to_list(X))++"}";
serialize(X) when is_list(X) -> "["++serializeList(X)++"]".

serializeList([]) -> "";
serializeList([X|Xs]) -> serialize(X)++case Xs of
                                         [] -> "";
                                         _ -> ","++serializeList(Xs)
                                       end.

deserialize(Str) ->
  {ok,Tokens,_EndLine} = erl_scan:string(Str++"."),
  {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
  {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  Value.

