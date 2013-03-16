%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. 関数の構文
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(functions).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.1. パターンマッチ %%%

%% 関数の基本について
%% functionとかdefとか何も言わずにおもむろに関数を書く.
greet(male, Name) ->
  io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).
%% _ はちょう大事！elseと似たような感じですべてをマッチさせる受け皿.


head([H|_]) -> H.
second([_,X|_]) -> X.

% 2> functions:head([1,2,3]).
% 1
% 3> functions:second([1,2,3]).
% 2

%% > Erlangでは、束縛されている変数に値を与えようとすると、新しい値が束縛されている値と同じでない限りエラーが発生します。
%% やってみよう.

%% 引数は二重タプル
valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").


%%%%%%%%%%%%%%%%%%%
%%% 5.2. ガード %%%

old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

%% , (カンマ)     は andalso... Rubyで言うと &&
%% ; (セミコロン) は orelse...  Rubyで言うと ||


