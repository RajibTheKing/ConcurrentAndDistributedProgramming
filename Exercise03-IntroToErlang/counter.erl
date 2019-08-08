-module(counter).
-export([startN/1, start/2, tick/2]).

% Start n counter for a list containing n counter delays
startN([Delay]) -> start(0,Delay);
startN([Delay|Delays]) ->
  spawn_link(fun() -> start(0,Delay) end),
  startN(Delays).

% Start a Counter with a start value and a delay
start(Value, Delay) ->
  Gui = counterGui:start(Value, self()),
  process_flag(trap_exit, true),
  Me = self(),
  spawn_link(fun() -> tick(Delay,Me) end),
  counter(Value, Delay, [Gui], true).

% Main process of a counter
counter(_, _, [], _) -> exit('No More counters left'); % exit(Reason) to stop the timer
counter(Value, Delay, Guis, Active) ->
  receive
    start ->
      base:printLn("Start action received"),
      counter(Value, Delay, Guis, true);
    stop  ->
      counter(Value, Delay, Guis, false);
    copy  ->
      spawn_link(fun() -> start(Value, Delay)end),
      counter(Value, Delay, Guis, Active);
    clone ->
      Gui = counterGui:start(Value, self()),
      counter(Value, Delay, [Gui|Guis], Active);
    {close,GuiPid} ->
      counter(Value, Delay, remove(GuiPid,Guis), Active);
    tick ->
      case Active of
        true ->
          %base:printLn("Got a tick "),
          sendAll({setValue,Value+1},Guis),
          counter(Value+1, Delay, Guis, Active);
        false ->
          counter(Value, Delay, Guis, Active)
      end;
    {'EXIT',_,_} ->
      counter(Value, Delay, Guis, Active);
    M -> io:format("Unknown Message: ~w~n",[M]),
      counter(Value, Delay, Guis, Active)
  end.

% Timer
tick(Delay, Pid) ->
  timer:sleep(Delay),
  %base:printLn("tick notification , sending to Pid"),
  Pid!tick,
  tick(Delay,Pid).

% Send a message to all pids in a list
sendAll(_,[])-> ok;
sendAll(Msg,[Pid|Pids]) ->
  Pid!Msg,
  sendAll(Msg,Pids).

% Remove an element of a list
remove(_,[])     -> [];
remove(X,[X|XS]) -> XS;
remove(X,[Y|XS]) -> [Y|remove(X,XS)].