-module(linda).
-export([start/0,in/2,rd/2,out/2,in/3,out/3,rd/3]).

start() -> spawn(fun() -> linda([],[]) end).
showServerState(TS,Requests) ->
  base:printLn(base:show(TS)),
  base:printLn(base:show(Requests)).

linda(TS,Requests) ->
  showServerState(TS,Requests),
  receive
    {out,T,Timeout}  -> case find_matching_requests(T,Requests) of
                  {true,New_Requests} -> linda([{T,calc_max_time(Timeout)}|TS],New_Requests);
                  {false,New_Requests} -> linda(TS,New_Requests)
                end;
    {in,F,P,Timeout} -> case find_matching_tuple(TS,F) of
                  nothing -> linda(TS,[{in,F,P,calc_max_time(Timeout)}|Requests]);
                  {just,{Res,NTS}} -> P!{found,Res},
                    linda(NTS,Requests)
                end;
    {rd,F,P,Timeout} -> case find_matching_tuple(TS,F) of
                  nothing -> linda(TS,[{rd,F,P,calc_max_time(Timeout)}|Requests]);
                  {just,{Res,_}} -> P!{found,Res},
                    linda(TS,Requests)
                end
  end.


calc_max_time(nothing)  -> nothing;
calc_max_time({just,T}) -> {just,current_time() + T * 1000}.

current_time() ->
  {Megas,Seconds,Micros} = erlang:timestamp(),
  (Megas * 1000 * 1000 + Seconds) * 1000 * 1000 + Micros.

timedout(nothing) -> false;
timedout({just,Max_time}) -> Max_time < current_time().


find_matching_requests(_,[]) -> {true,[]};
find_matching_requests(T,[{Kind,F,P,Max_time}|Requests]) ->
  case find_matching_requests(T,Requests) of
    {false,New_Requests} -> {false,[{Kind,F,P,Max_time}|New_Requests]};
    {true,New_Requests}  ->
      case timedout(Max_time) of
        true -> find_matching_requests(T,Requests);
        false -> case catch F(T) of
                   {'EXIT',_} -> {true,[{Kind,F,P,Max_time}|New_Requests]};
                   Res        -> P!{found,Res},
                     case Kind of
                       in -> {false,New_Requests};
                       rd -> {true ,New_Requests}
                     end
                 end
      end
  end.



find_matching_tuple([],_) -> nothing;
find_matching_tuple([{T,Max_time}|Ts],F) ->
  case timedout(Max_time) of
    true -> find_matching_tuple(Ts,F);
    false -> case catch F(T) of
               {'EXIT',_} -> case find_matching_tuple(Ts,F) of
                               nothing -> nothing;
                               {just,{FoundT,NTs}} -> {just,{FoundT,[{T,Max_time}|NTs]}}
                             end;
               Res        -> {just,{Res,Ts}}
             end
  end.



% Interface

out(TS,T) -> TS!{out,T,nothing}.
out(TS,T,Timeout) -> TS!{out,T,{just,Timeout}}.

in(TS,P) -> TS!{in,P,self(),nothing},
  receive
    {found,Res} -> Res
  end.

rd(TS,P) -> TS!{rd,P,self(),nothing},
  receive
    {found,Res} -> Res
  end.


in(TS,P,Timeout) -> TS!{in,P,self(),{just,Timeout}},
  receive
    {found,Res} -> {just,Res}
  after Timeout -> nothing
  end.
  
rd(TS,P,Timeout) -> TS!{rd,P,self(),{just,Timeout}},
  receive
    {found,Res} -> {just,Res}
  after Timeout -> nothing
  end.


