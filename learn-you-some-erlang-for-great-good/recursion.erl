%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 7. 再帰
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(recursion).
-export([fac/1,len/1,tail_len/1,duplicate/2,tail_duplicate/2,tail_reverse/1,sublist/2,tail_sublist/2,zip/2,quicksort/1,lc_quicksort/1]).

% fac(N) when N == 0 -> 1;
% fac(N) when N > 0 -> N*fac(N-1).

%% もっと短く書けるよ

fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).


%%% 7.3. 末尾再帰 %%%
%% > 末尾再帰は上に書いた線形の処理（要素が増えれば増えるほど大きくなる処理）を逐次的な処理（全然大きくならない処理）に変更します。 関数の呼び出し方を末尾再帰にするには、それを「ひとつだけ」にすることです。
%% > 違いはなんでしょうか？いまは2つ以上の項をメモリに持つ必要がありません。 メモリ使用量は一定です。4の階乗でも100万の階乗でも同じスペースしか使いません。

%% その時その時のresultを溜めて引き連れつつ再帰するんだったね.

%% 呼び出し階層をうまいこと表示したい...
len([]) -> 0;
len([_|T]) ->
  io:format("  "),
  io:format("1 + len(~p)\n", [T]),
  io:format("1 + ~p\n", [len(T)]),
  1 + len(T).

%% ともかく末尾再帰ver.
%% 引数の数 & []の要素数で場合分けしてやるよ
%% まずリストだけ渡されたら内部の関数(2個引数)を読んでやる. 公開するのは1個verのみ.
tail_len(L) -> tail_len(L,0).

tail_len([], Acc) -> Acc; %% カラになったら溜めてきた答えを提出.
tail_len([_|T], Acc) ->
  io:format("len:~p -- ~p\n", [Acc,T]),
  tail_len(T, Acc+1).

%    4> recursion:tail_len([1,2,3,4,5]).
%    len:0 -- [2,3,4,5]
%    len:1 -- [3,4,5]
%    len:2 -- [4,5]
%    len:3 -- [5]
%    len:4 -- []
%    5


duplicate(0,_) -> [];
duplicate(N,X) when N > 0 ->
  [X|duplicate(N-1,X)].

%% 自分で書いてみたが, when N > 0入れるの忘れてた.

%% 次, 末尾再帰. まず 引数2個の公開用関数, その中で途中経過を渡しつつ.
tail_duplicate(N,Term) ->
  tail_duplicate(N,Term,[]).
tail_duplicate(0,_,List) ->
  List;
tail_duplicate(N,Term,List) when N > 0 ->
  tail_duplicate(N-1,Term,[Term|List]).

%    tail_duplicate(0,_,List) ->
%      List. %% <= これはエラー "function tail_duplicate/3 already defined"
%    tail_duplicate(N,Term,List) when N > 0 ->
%      tail_duplicate(N-1,Term,[Term|List]).
%
%    tail_duplicate(0,_,List) ->
%      List; %% <= semicolonにすればおｋ
%    tail_duplicate(N,Term,List) when N > 0 ->
%      tail_duplicate(N-1,Term,[Term|List]).

tail_reverse(L) -> tail_reverse(L,[]).
tail_reverse([],Result) -> Result;
tail_reverse([HD|TL],Result) ->
  tail_reverse(TL,[HD|Result]).

%% takeを作る.
sublist(_,0) -> [];
sublist([],_) -> [];
sublist([H|T],N) when N > 0 -> [H|sublist(T,N-1)].

%% 末尾再帰版. 最後にreverseするの注意
tail_sublist(L,N) ->
  tail_reverse(tail_sublist(L,N,[])). %% 最後にreverseしないといけない
tail_sublist(_,0,SubList) -> SubList;
tail_sublist([],_,SubList) -> SubList;
tail_sublist([H|T],N,SubList) ->
  tail_sublist(T,N-1,[H|SubList]).


zip(_,[]) -> [];
zip([],_) -> [];
zip([X|Xs],[Y|Ys]) ->
  [{X,Y}|zip(Xs,Ys)].

%% > ここで見た末尾再帰はメモリ消費量を大きくしません。 なぜなら仮想マシンが関数が自分自身を末尾で呼び出すからです。（関数内で最後の式が評価される） これで現在のスタックフレームを削除します。 これは末尾呼び出し最適化（Tail Call Optimization, TCO）と呼ばれ、終端呼び出し最適化（Last Call Optimization, LCO）と呼ばれる最適化の特殊なケースです。
%% > LCOは関数本体で評価される最後の式が他の関数を呼び出す場合に行われます。 TCOが行われると、Erlang VMはスタックフレームに保存するのを避けます。 末尾再帰は複数の関数の間でも可能です。例として、 a() -> b(), b() -> c(), c() -> a() のような関数の連鎖があるとします。 このときにLCOがスタックオーバーフローを避けてるようにするので、この無限ループはメモリ不足にならず効率的に動作します。 この原則から、アキュムレータを使うことによって末尾再帰が有用なものになるのです。

%% quicksort! -- 分解していく
quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
  {Smaller, Larger} = partition(Pivot,Rest,[],[]),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).


%% Restがカラになったらgoal.
partition(_,[],Smaller,Larger) -> {Smaller,Larger};
partition(Pivot,[H|T],Smaller,Larger) ->
  %% 最初の1個を取って大小調べ,SmallerかLargerに加えて再帰呼び出し
  if H =< Pivot -> partition(Pivot,T,[H|Smaller],Larger);
    H > Pivot -> partition(Pivot,T,Smaller,[H|Larger])
  end.

%% 内包リストをつかったバージョン.
%% > ずっと読みやすくなった代わりに、リストを2つに分けるために一旦リストを走査してやらなければいけません。
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
  %% Erlangの || ってなんだっけ
  lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
  ++ [Pivot] ++
  lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).

