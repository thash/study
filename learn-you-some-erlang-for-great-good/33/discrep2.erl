-module(discrep2).
-export([run/0]).

run() ->
  Tup = money(5, you),
  some_op(count(Tup), account(Tup)).

money(Num, Name) -> {give, Num, Name}.
count({give, Num, _}) -> Num.
account({give, _, X}) -> X.

some_op(A, B) -> A + B.

% neon $ dialyzer discrep2.erl
%   Checking whether the PLT /Users/hash/.dialyzer_plt is up-to-date... yes
%   Proceeding with analysis...
% discrep2.erl:4: Function run/0 has no local return
% discrep2.erl:6: The call discrep2:some_op(5,'you') will never return since it differs in the 2nd argument from the success typing arguments: (number(),number())
% discrep2.erl:12: Function some_op/2 has no local return
% discrep2.erl:12: The call erlang:'+'(A::5,B::'you') will never return since it differs in the 2nd argument from the success typing arguments: (number(),number())
%  done in 0m0.66s
% done (warnings were emitted)
