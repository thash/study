%%% 13.4. 選択的receive %%%
%% sleepやflushもこのreceive+afterで実装されてるよ.
%% flushはafter 0をつかってるんだが, このafter 0を利用して階層的なreceiveができる.

-module(multiproc).
-compile(export_all).

important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
  after 0 ->
      normal()
  end.

normal() ->
  receive
    {_, Message} ->
      [Message | normal()]
  after 0 ->
      []
  end.

% 1> c(multiproc).
% 2> self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}.
% {17,high}
% 3> multiproc:important().
% [high,high,low,low]

%% メッセージはFIFO, 古いものから順に処理される.

