-module(cards).
-export([kind/1, main/0]).

-type suit() :: spades | clubs | hearts | diamonds.
-type value() :: 1..10 | j | q | k.
-type card() :: {suit(), value()}.

-spec kind(card()) -> face | number.
kind({_, A}) when A >= 1, A =< 10 -> number;
kind(_) -> face.

main() ->
  number = kind({spades, 7}),
  face = kind({hearts, k}),
  number = kind({rubies, 4}), %?? => このままだと警告してくれない
  face = kind({clubs, q}).

% $ dialyzer cards.erl
%   Proceeding with analysis... done in 0m0.62s
% done (passed successfully)

%% dialyzerも通ってしまう.
%% が, 8行目のspecを使えば検出できるように！

% $ dialyzer cards.erl
% cards.erl:12: Function main/0 has no local return
% cards.erl:15: The call cards:kind({'rubies',4}) breaks the contract (card()) -> 'face' | 'number'
%  done in 0m0.53s
% done (warnings were emitted)

%% R15Bバージョン以降は-callbackというモジュール属性(って言うのかこのハイフン始まりの宣言...)が追加された.
