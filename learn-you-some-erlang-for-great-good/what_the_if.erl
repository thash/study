%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. 関数の構文
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   5.3. If   %%%

-module(what_the_if).
-export([heh_fine/0]).

heh_fine() ->
  if 1 =:= 1 ->
      works
  end,
  if 1 =:= 2; 1 =:= 1 ->
    works
end,
if 1 =:= 2, 1 =:= 1 ->
    fails
end.

%% これをコンパイルするとWarningが出る.
%% what_the_if.erl:16: Warning: no clause will ever match
%% what_the_if.erl:16: Warning: the guard for this clause evaluates to 'false'

%% 16行目はfalseしか返さないif式なのであかんでー, とのこと.
%% 確かにVimのsyntasticも警告吐いてるし.

%% 警告が出る本質的理由は, Erlangはすべてのものは何かを返さないといけないのに,
%% heh_fineは"何も返せない"から. とのこと.

%% ここでelseですよ. パターンマッチの _ をelseと書いたけど先走ったようです.
%% Erlangにおけるif式のelseはtrue branchという!
%% なんか作者? は「そもそもelse(;true ->)は避けるべきだよ」と言ってるらしいがね.
%% プログラミング思想として, ifを書くときはすべての場合を網羅しろってことらしい. はー.

oh_god(N) ->
  if N =:= 2 -> might_suceed;
    true -> always_does %% this is Erlang's if's 'else'
  end.

%% if 式はend.で閉じましょう.


%%%   5.4. case   %%%
%% if式はガードっぽく, case...of式は関数っぽい!
%% 関数の中に関数が入っている...ような?

beach(Temperature) ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 ->
      'favorable';
    {kelvin, N} when N >= 293, N =< 318 ->
      'scintifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 ->
      'favorable in the US';
    _ ->
      'avoid beach'
  end.

