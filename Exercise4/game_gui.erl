-module(game_gui).

-import(base,[print/1,printLn/1,show/1]).

-export([start/3, counterWindow/4]).

-define(BUTTON,1).
-define(FRAME,2).
-define(TEXT,3).

start(ClientPid,X,Y) ->
  spawn(fun() -> counterWindow(ClientPid,X,Y, none) end).

counterWindow(ClientPid,X,Y, Name) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx,?FRAME,"Gamer Handle: " ++ base:show(Name),[{size,{X,Y}}]),
  Panel = wxPanel:new(Frame),
  Text = wxStaticText:new(Panel,?TEXT, ""),
  wxFrame:connect(Frame,command_button_clicked),
  wxFrame:connect(Frame,close_window),
  wxFrame:show(Frame),
  loop(false, false, ClientPid, Panel, false, Text).

loop(Button, ID, ClientPid, Panel, ButtonExists,Text) ->
  receive
    {text_update,NewText} ->
      %Update TextLabel
      wxStaticText:setLabel(Text,NewText),
      loop(Button, ID, ClientPid, Panel, ButtonExists, Text);
    {button_create,I,X,Y} ->
      %Create a new button, delete an old one, if necessary
      case ButtonExists of
        true -> wxButton:destroy(Button);
        false -> ok
      end,
      NewButton = wxButton:new(Panel, ?BUTTON, [{label,"Click"},{pos,{X,Y}},{size,{100,50}}]),
      wxButton:connect(NewButton, command_button_clicked, [{callback,
         fun(_, _) ->
             ClientPid ! {clicked, ID},
             base:printLn("TheKing --> Button is clicked.. click~n  event = ~p~n  obj = ~p~n")
             end
         }]),
      loop(NewButton, I, ClientPid, Panel,true,Text);
    button_remove ->
      %Remove the button
      case ButtonExists of
        true -> wxButton:destroy(Button);
        false -> ok
      end,
      loop(false, false, ClientPid, Panel, false, Text);
    {wx, ?BUTTON, _, _, _} ->
      %Send a message to the Client, if the the Button was pressed
      ClientPid ! {clicked,ID},
      loop(Button, ID, ClientPid, Panel, ButtonExists, Text);
    {wx, ?FRAME, _, _, _} ->
      %Send close to the Client, if the Gui gets closed
      ClientPid ! close,
      exit(-1);
    Msg -> printLn("Unknown message: "++show(Msg)),
      loop(Button, ID, ClientPid, Panel, ButtonExists, Text)
  end.
