-module(translate).
-export([loop/0, translate/2]).

loop() ->
  receive
    % "casa" ->
    {From, "casa"} -> % 送り手Pidを使うver.
      From ! "got it", % メッセージを送り返す.
      io:format("house~n"),
      loop();
    "blanca" ->
      io:format("white~n"),
      loop();
    _ ->
      io:format("I don't understand.~n"),
      loop()
end.

translate(To, Word) ->
  % 送信メッセージに自分自身(のPid)を含める. loop内でFromとして受け取っている所.
  To ! {self(), Word},
  receive
    Translation -> Translation
  end.
