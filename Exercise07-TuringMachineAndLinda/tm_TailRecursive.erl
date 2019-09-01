-module(tm_TailRecursive).
-export([startTM/1, startTM/0, tape/4, test/0]).

% The process loop for a tape cell.
tape(SL,SR,P,E) ->
  receive
    read ->
      P!E,
      tape(SL,SR,P,E);
    left ->
      case SL of
        none -> 
          NewSL = newTapeCell(none,self(),P,blank),
          P!NewSL,
          tape(NewSL,SR,P,E);
        _Pid ->
          P!SL,
          tape(SL,SR,P,E)
      end;
    right ->
      case SR of
        none ->
          NewSR = newTapeCell(self(),none,P,blank),
          P!NewSR,
          tape(SL,NewSR,P,E);
        _Pid ->
          P!SR,
          tape(SL,SR,P,E)
      end;
    V ->
      P!written,
      tape(SL,SR,P,V)
  end.

% Create a new tape cell.
newTapeCell(SL,SR,P,E) -> spawn(fun() -> tape(SL,SR,P,E) end).

% Create a new tape cell containing a blank.
newBlankTape(P) -> newTapeCell(none,none,P,blank).

read(TP) ->
    TP ! raed,
    receive
        Ans -> Ans
    end.

write(TP, V) ->
    TP ! V,
    receive
        V -> ok
    end.

% Move the head of the tape.
move(Tape,   l) -> Tape!left , receive TL -> TL end;
move(Tape,   r) -> Tape!right, receive TR -> TR end;
move(Tape,_Dir) -> Tape.



% Write a word on the tape.
writeWord(_Tape,    []) -> ok;
writeWord( Tape,[X|XS]) -> write(Tape,X), writeWord(move(Tape,r),XS).


% Start the turing machine with the empty word.
startTM() ->
  Tape = newBlankTape(self()),
  runTM(Tape,q0).

% Start the turing machine with a given word.
startTM(Word) ->
  Tape = newBlankTape(self()),
  writeWord(Tape,Word),
  runTM(Tape,q0).

final(qY) -> true;
final(qN) -> true;
final(_Q) -> false.

% Control process of a turing machine.
runTM(Tape,Q) ->
  case final(Q) of
    true  -> Q;
    false ->
      Value = read(Tape),
      {Q2,NewValue,Dir} = delta(Q,Value),
      write(Tape,NewValue),
      runTM(move(Tape,Dir),Q2)
  end.

% test of word "aabb"
test() -> startTM(['a','a','b','b']).


% A concrete turing machine accepting the word a^nb^n, n>=0.
delta(q0,'a')   -> {q1,blank,r};
delta(q0,blank) -> {qY,blank,r};
delta(q1,'a')   -> {q1,'a',r};
delta(q1,'b')   -> {q2,'b',r};
delta(q2,'b')   -> {q2,'b',r};
delta(q2,blank) -> {q3,blank,l};
delta(q3,'b')   -> {q4,blank,l};
delta(q4,'b')   -> {q4,'b',l};
delta(q4,'a')   -> {q4,'a',l};
delta(q4,blank) -> {q0,blank,r};
delta(_Q,_X)    -> {qN,blank,r}.
