%%% $ dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl common_test eunit
%% dialyzerなどというコマンドがいつの間にやら.
%% 実行したらなんか頑張り始めた...

% Unknown functions:
% ...ずらずらと関数リスト...
% Unknown types:
%   compile:option/0
%   ct:hook_options/0
%   inet:host_name/0

%% ~/に.dialyzer_ptl的なものを勝手に生成しやがって邪魔臭いけど.
%% これを消してしまうと自作コードの解析ができなくなる.

%% ちなみにdialyzerは楽観的なので, エラーが起こる可能性のある以下のようなコードは警告しない.
main() ->
  X = case fetch() of %% fetch()は1か2を返す.
     1 -> some_atom;
     2 -> 3.14
   end,
   convert(X).

convert(X) when is_atom(X) -> {atom, X}.

%% Dialyzerは型エラーを見つけるためだけのコマンド.
