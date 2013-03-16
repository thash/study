%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 28. Bears, ETS, Beets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% いままで, 何らかのデータをプロセスに保持させてきた.
%% その設計自体は悪いことじゃないが, "他のプロセスとデータを共有する" 時はより適した方法を採用する必要がある.
%% たとえば ETS(Erlang Term Strage)テーブル. etsモジュールのBIFとして実装されている.
%%    * BIF = Built in Function:組込み関数. 純粋なErlangではなく他の言語(C)で実装されている. 例: lists:reverse
%% これはErlang VMに含まれるインメモリデータベース. GCは(あえて)行われない.

% 1> ets:new(ingredients, [set, named_table]).
% ingredients
% 2> ingredients.
% ingredients
% 3> ets:insert(ingredients, {bacon, great}).
% 5> ets:lookup(ingredients, bacon).
% [{bacon,great}] <= リストを返す.

%% 高階パターンマッチング ... Erlangでは利用できません.
%% > 代わりにErlangには特殊言語があって, パターンマッチングを大量の標準データ構造として表現するために使われています.

% 6> ets:new(table, [named_table, bag]).
% 7> ets:insert(table, [{items, a, b, c, d}, {items, a, b, c, a}, {cat, brown, soft, loveable, selfish}, {friends, [jenn,jeff,etc]}, {items, 1, 2, 3, 1}]).
% 8> ets:match(table, {items, '$1', '$2', '_', '$1'}).
% [[a,b],[1,2]]

%% これをさらに強化した, マッチスペックという気の狂った文法のパターンマッチがあるけど省略.

