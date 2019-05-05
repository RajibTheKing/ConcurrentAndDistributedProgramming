-module(counterGui).

-export([start/2, counterWindow/2, main/0, outputMessages/0]).
-include_lib("wx/include/wx.hrl").

-define(START,1).
-define(STOP,2).
-define(COPY,3).
-define(CLONE,4).
-define(CLOSE,5).
-define(TEXT,6).
-define(FRAME,7).

start(V,ClientPid) ->
  spawn(fun() -> counterWindow(V,ClientPid) end).

counterWindow(V,ClientPid) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx,?FRAME,"Counter"),
  Panel = wxPanel:new(Frame),
  StartButt = wxButton:new(Panel, ?START, [{label,"Start"}]),
  StopButt = wxButton:new(Panel, ?STOP, [{label,"Stop"}]),
  CopyButt = wxButton:new(Panel, ?COPY, [{label,"Copy"}]),
  CloneButt = wxButton:new(Panel, ?CLONE, [{label,"Clone"}]),
  CloseButt = wxButton:new(Panel, ?CLOSE, [{label,"Close"}]),
  Text = wxStaticText:new(Panel,?TEXT, integer_to_list(V)),
  wxFrame:connect(Frame,command_button_clicked),
  wxFrame:connect(Frame,close_window),

  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  OuterSizer = wxBoxSizer:new(?wxVERTICAL),
  ValueSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(ButtonSizer,StartButt, []),
  wxSizer:add(ButtonSizer,StopButt, []),
  wxSizer:add(ButtonSizer,CopyButt, []),
  wxSizer:add(ButtonSizer,CloneButt, []),
  wxSizer:add(ButtonSizer,CloseButt, []),
  wxSizer:add(ValueSizer,Text,[]),
  wxSizer:add(OuterSizer,ValueSizer,[]),
  wxSizer:add(OuterSizer,ButtonSizer,[]),
  wxPanel:setSizer(Panel,OuterSizer),
  wxFrame:show(Frame),

  loop(Text, StartButt, StopButt, CopyButt, CloneButt, ClientPid, CloseButt).

loop(Text, StartButt, StopButt, CopyButt, CloneButt, ClientPid, CloseButt) ->
  receive
    {setValue, V} ->
      wxStaticText:setLabel(Text,integer_to_list(V)),
      loop(Text, StartButt, StopButt, CopyButt, CloneButt,
        ClientPid, CloseButt);
    {newClient, P} ->
      loop(Text, StartButt, StopButt, CopyButt, CloneButt, P,
        CloseButt);
    {wx, ?START, _, _, _} ->
      ClientPid ! start,
      loop(Text, StartButt, StopButt, CopyButt, CloneButt,
        ClientPid, CloseButt);
    {wx, ?STOP, _, _, _} ->
      ClientPid ! stop,
      loop(Text, StartButt, StopButt, CopyButt, CloneButt,
        ClientPid, CloseButt);
    {wx, ?COPY, _, _, _} ->
      ClientPid ! copy,
      loop(Text, StartButt, StopButt, CopyButt, CloneButt,
        ClientPid, CloseButt);
    {wx, ?CLONE, _, _, _} ->
      ClientPid ! clone,
      loop(Text, StartButt, StopButt, CopyButt, CloneButt,
        ClientPid, CloseButt);
    {wx, ?CLOSE, _, _, _} ->
      ClientPid ! {close, self()},
      exit(-1);
    {wx, ?FRAME, _, _, _} ->
      ClientPid ! {close, self()},
      exit(-1);
    _ -> loop(Text, StartButt, StopButt, CopyButt, CloneButt,
      ClientPid, CloseButt)
  end.

% Just for testing

main() ->
  P = spawn(counterGui, outputMessages, []),
  spawn(counterGui, start, [3, P]).

outputMessages() ->
  receive
    closeAll ->
      io:format("Message: ~w~n", [closeAll]);
    M ->
      io:format("Message: ~w~n", [M]),
      outputMessages()
  end.


