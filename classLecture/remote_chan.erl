-module(remote_chan).
-export([new_chan/0,chan_register/3,chan_lookup/2,
         read_chan/1,write_chan/2]).

new_chan() ->
    {ok,LSock} = gen_tcp:listen(0, [list, {packet,line}, {reuseaddr,true}]),
    {ok,Port} = inet:port(LSock),
    Chan = {local_chan,spawn(fun() -> chan() end),{127,0,0,1},Port},
    spawn(fun() -> portListener(LSock,Chan) end),
    Chan.

portListener(LSock,Chan) ->
  case gen_tcp:accept(LSock) of
    {ok,Sock} ->
      receive
        {tcp,Sock,MsgStr} ->
           {remote_msg,Msg} = deserialize(MsgStr),
           write_chan(Chan,Msg),
           gen_tcp:close(Sock),
           portListener(LSock,Chan)
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
                                 chan()
                end
  end.

read_chan({local_chan,P,_,_}) ->
  P!{read,self()},
  receive
    {chanMsg,Msg} -> Msg
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


