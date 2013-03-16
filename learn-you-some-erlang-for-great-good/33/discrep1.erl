-module(discrep1).
-export([run/0]).

run() -> some_op(5, you).
some_op(A, B) -> A + B.

%% このコードをdializerにかけてみると

% $ dialyzer discrep1.erl

%   Checking whether the PLT /Users/hash/.dialyzer_plt is up-to-date... yes
%   Proceeding with analysis...
% discrep1.erl:4: Function run/0 has no local return
% discrep1.erl:4: The call discrep1:some_op(5,'you') will never return since it differs in the 2nd argument from the success typing arguments: (number(),number())
% discrep1.erl:5: Function some_op/2 has no local return
% discrep1.erl:5: The call erlang:'+'(A::5,B::'you') will never return since it differs in the 2nd argument from the success typing arguments: (number(),number())
%  done in 0m0.62s
% done (warnings were emitted)
