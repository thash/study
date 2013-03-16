-module(discrep3).
-export([run/0]).

run() ->
  Tup = money(5, you),
  some_op(item(count,Tup), item(account,Tup)).

money(Num, Name) -> {give, Num, Name}.
item(count, {give, X, _}) -> X;
item(account, {give, _, X}) -> X.

some_op(A, B) -> A + B.

% neon $ dialyzer discrep3.erl
%   Checking whether the PLT /Users/hash/.dialyzer_plt is up-to-date... yes
%   Proceeding with analysis... done in 0m0.61s
% done (passed successfully)
