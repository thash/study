-module(dolphins).
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip ->
      io:format("How about no?~n");
    fish ->
      io:format("So long and thanks for all the fish!~n");
    _ ->
      io:format("Heh, we're smarter than you humans.~n")
  end.

%% ガードも使えるよ
% receive
%     Pattern1 when Guard1 -> Expr1;
%     Pattern2 when Guard2 -> Expr2;
%     Pattern3 -> Expr3
%   end.

%% spwnしつつそれへのアクセス(?)をDolphin変数に束縛, メッセージを渡す.
%% > spawn/3 は1つの関数だけを引数に取るのではなく、モジュール、関数とその引数を引数に取ります。
% 2> Dolphin = spawn(dolphins, dolphin1, []).
% <0.39.0>
% 3> Dolphin ! "oh, hello !".
% Heh, we're smarter than you humans.
% "oh, hello !"
% 4> Dolphin ! fish.
% fish


%% > 関数が一度 “Heh, we’re smarter than you humans.” という出力をしたら、関数はそれでおしまいで、プロセスもそこで終わるということです。 イルカを再起動する必要があります。
% 5> f(Dolphin).
% ok
% 6> Dolphin ! fish. %% f/1はunbound? そういえばErlangは変数への再代入ができない.
% * 1: variable 'Dolphin' is unbound
% 7> Dolphin = spawn(dolphins, dolphin1, []).
% <0.45.0>
% 8> Dolphin ! fish.
% So long and thanks for all the fish!
% fish

%% このままじゃ送りっぱなしでコミュニケーションできないから送り主をtapleに詰めてやりとりするようにしましょう.
%% つまり fish を送るんじゃなくて {self(), fish} を送る.
%% 受け手の関数をつくろう. せっかく送り主がわかるようになったので From に対して文字列を送り返すよ. io:formatじゃない.
dolphin2() ->
  receive
    {From, do_a_flip} ->
      From ! "How about no?";
    {From, fish} ->
      From ! "So long and thanks for all the fish!";
    _ ->
      io:format("Heh, we're smarter than you humans.~n")
  end.


% 2> Dolphin2 = spawn(dolphins, dolphin2, []).
% <0.39.0>
% 3> Dolphin2 ! {self(), do_a_flip}.
% {<0.32.0>,do_a_flip}
% 4> flush().
% Shell got "How about no?"
% ok
%% Dolphin2から返事が帰ってきた！


%% まだいちいちDolphinを起動する必要がある.
%% じゃあ次は再帰でぐるぐる回そう. 魚もらったら満足して止まる設定.

dolphin3() ->
  receive
    {From, do_a_flip} ->
      From ! "How about no?",
      dolphin3();
    {From, fish} ->
      From ! "So long and thanks for all the fish!";
    _ ->
      io:format("Heh, we're smarter than you humans.~n"),
      dolphin3() %% 条件分岐の最後は,も;もない
end.


% 2> Dolphin3 = spawn(dolphins, dolphin3, []).
% <0.38.0>
% 3> Dolphin3 ! Dolphin3 ! {self(), do_a_flip}.
% {<0.31.0>,do_a_flip}
% 4> flush().
% Shell got "How about no?"
% Shell got "How about no?"
% ok
% 5> Dolphin3 ! {self(), unknown_message}.
% Heh, we're smarter than you humans.
% {<0.31.0>,unknown_message}
% 6> flush().
% ok
% 7> Dolphin3 ! Dolphin3 ! {self(), fish}.
% {<0.31.0>,fish}
% 8> flush()
% Shell got "So long and thanks for all the fish!"
% ok

