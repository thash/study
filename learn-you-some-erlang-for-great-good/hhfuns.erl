%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 8. 高階関数(Higher Order Functions)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% > すべての関数型プログラミング言語での重要なのは定義した関数を持ってきて他の関数へパラメータとして渡すことができる、という点です。

-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().

% > 2> hhfuns:add(one,two).
% > ** exception error: bad function one
% >      in function  hhfuns:add/2
% > 3> hhfuns:add(1,2).
% > ** exception error: bad function 1
% >      in function  hhfuns:add/2
% > 4> hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
% > 3

%% > 関数名がパラメータなしで書かれた場合, それらの名前はアトムとして解釈されます.
%% > そしてアトムは関数になれません.
%% そして関数をモジュールの外から"もの"として扱うために
%%     fun module:function/arity
%% という記法が導入されたわけ.


%% この記法が役立つ例を見てみよう. 次の２つの関数はほとんど同じ事をやっている.

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].


map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

%    6> L = [1,2,3,4,5].
%    [1,2,3,4,5]
%    7> hhfuns:increment(L).
%    [2,3,4,5,6]
%    8> hhfuns:map(fun hhfuns:incr/1, L).
%    [2,3,4,5,6]


%%% 8.2. 無名関数 %%%
%% funで作る. 再帰呼び出し以外ならなんでもできるよ！

%    9> Fn = fun() -> a end.
%    #Fun<erl_eval.20.21881191>
%    10> Fn().
%    a

%% クロージャ %%

%     PrepareAlerm = fun(Room) ->
%         io:format("Alerm set in ~s. ~n", [Room]),
%         fun() -> io:format("Alerm tripped in ~s! Call Batman!~n", [Room]) end
%     end.

%     #Fun<erl_eval.6.80247286>
%     6> AlermReady = PrepareAlerm("bathroom").
%     Alerm set in bathroom.
%     #Fun<erl_eval.20.21881191>
%     7> AlermReady().
%     Alerm tripped in bathroom! Call Batman!
%     ok

%% 作ってその場で呼び出す.
base(A) ->
  B = A + 1,
  F = fun() -> A * B end,
  F().

%% 作って保持. 関数を返す.
base2(A) ->  B = A + 1,  fun() -> A * B end.

%     7> HH = hhfuns:base2(2).
%     #Fun<hhfuns.1.51278220>
%     8> HH().
%     6


%% > 継承されたスコープは無名関数がどこにあっても見えている.
%% 無名関数の"定義時"に参照できたのであれば,
%% 無名関数を"呼び出す"のがどこであろうと参照可能である.

a() ->
  Secret = "poyb",
  fun() -> Secret end.

b(F) ->
  "a/0's password is " ++ F().

%% 見えちゃう.
%    11> hhfuns:b(hhfuns:a()).
%    "a/0's password is poyb"

%% 呼び出し方を間違えた(funをつけた)らなにこの結果!?
%    10> hhfuns:b(fun hhfuns:a/0).
%    [97,47,48,39,115,32,112,97,115,115,119,111,114,100,32,105,
%     115,32|#Fun<hhfuns.2.131884022>]


%%% 8.3. map, filter, fold %%%

even(L) -> lists:reverse(even(L,[])).
even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
  even(T, [H|Acc]);
even([_|T], Acc) ->
  even(T, Acc).

%% filterで一般化しよう
filter(Pred, L) -> lists:reverse(filter(Pred,L,[])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
end.

%     8> Numbers = lists:seq(1,10).
%     [1,2,3,4,5,6,7,8,9,10]
%     9> hhfuns:filter(fun(X) -> X rem 2 == 0 end, Numbers).
%     [2,4,6,8,10]


%% foldもつくろう %%
%% 具体例
max([H|T]) -> max2(T,H).

max2([],Max) -> Max;
max2([H|T],Max) when H > Max -> max2(T,H);
max2([_|T],Max) -> max2(T,Max).

sum(L) -> sum(L,0).
sum([],Sum) -> Sum;
sum([H|T],Sum) -> sum(T,H+Sum).


%% 一般化
fold(_,Start,[]) -> Start;
fold(F,Start,[H|T]) -> fold(F,F(H,Start),T).

%     6> [H|T] = [1,7,3,5,9,0,2,3].
%     9> hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T). %% max
%     9
%
%     10> hhfuns:fold(fun(A,B) -> A + B end, H, T). %% sum
%     30

%% reverseもfilterも! foldで実装できる
%% fold第一引数に渡す関数は引数ふたつ.
reverse(L) ->
  fold(fun(X,Acc) -> [X|Acc] end, [], L).

map2(F,L) ->
  reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

filter2(Pred,L) ->
  F = fun(X,Acc) ->
      case Pred(X) of
        true -> [X|Acc];
        false -> Acc
      end
  end,
  reverse(fold(F,[],L)).

%% リストのドキュメントは必読
%% http://erldocs.com/R14B/stdlib/lists.html

