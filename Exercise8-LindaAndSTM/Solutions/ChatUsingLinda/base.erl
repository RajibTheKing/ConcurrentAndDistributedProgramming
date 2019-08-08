-module(base).
-export([print/1,printLn/1,getLn/0,getLn/1,show/1,
         lookup/2]).

% Gibt einen String aus.
% Auch andere Datenstrukturen können ausgegeen werden, allerdings
% keine Integer-Listen .
print("") -> "";
print(Str) when is_list(Str), is_integer(hd(Str)) -> io:fwrite("~s",[Str]);
print(Data) -> print(show(Data)).

% Wie print, aber zusätzlich wird ein Zeilenumbruch ausgegeben.
printLn(Str) -> print(Str),io:nl().

% getLn() liest eine Zeile von der Tastatur ein.
getLn() -> getLn("").

% getLn(Prompt) liest eine Zeile von der Tastatur ein.
% Das Atom P wird als Prompt verwendet.
getLn(P) -> L = io:get_line(list_to_atom(P)),
	    {Res,_} = lists:split(length(L)-1,L),
	    Res.

show(X) when is_atom(X) -> atom_to_list(X);
show(X) when is_integer(X) -> integer_to_list(X);
show(X) when is_pid(X) -> pid_to_list(X);
show(X) when is_tuple(X) -> "{"++showList(tuple_to_list(X))++"}";
show(X) when is_list(X) -> case allInteger(X) of
                          true -> "\""++X++"\"";
                          false -> "["++showList(X)++"]"
                        end;
show(X) when is_function(X) -> "FUN".

allInteger([]) -> true;
allInteger([X|Xs]) when is_integer(X) -> allInteger(Xs);
allInteger(_) -> false.

showList([]) -> "";
showList([X|Xs]) -> show(X)++case Xs of
                               [] -> "";
                               _ -> ","++showList(Xs)
                             end.

lookup(_,[])        -> nothing;
lookup(K,[{K,V}|_]) -> {just,V};
lookup(K,[_|KVs])   -> lookup(K,KVs).

