-module(ops_tests).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
  4 = ops:add(2,2).

%% テストが通った時
% 4> eunit:test(ops).
%   Test passed.
% ok

%% 通らなかった時
% ops_tests: add_test (module 'ops_tests')...*failed*
% in function ops_tests:add_test/0 (ops_tests.erl, line 5)
% **error:{badmatch,4}
% =======================================================
%   Failed: 1.  Skipped: 0.  Passed: 0.
% error

%% assert系のマクロがいくつかある. ?assertEqual(A,B) とか. 使ってみましょう.


new_add_test() -> %% 名前はなんでもいいらしい
  ?assertEqual(4, ops:add(2,2)),
  ?assertEqual(3, ops:add(1,2)),
  ?assert(is_number(ops:add(1,2))),
  ?assertEqual(3, ops:add(1,1)),
  ?assertError(badarith, 1/0).
%% badarithでコケるが大丈夫か？というWarningが出るけどc(ops_tests).ロードは出来る.

%% これをつかった時の出力
% ops_tests: new_add_test...*failed*
% in function ops_tests:'-new_add_test/0-fun-3-'/1 (ops_tests.erl, line 27)
% **error:{assertEqual_failed,[{module,ops_tests},
%                      {line,27},
%                      {expression,"ops : add ( 1 , 1 )"},
%                      {expected,3},
%                      {value,2}]}
%
%
% =======================================================
%   Failed: 1.  Skipped: 0.  Passed: 1.
% error


%%% 27.3. テストジェネレータ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 以下の2個は等価.
% function_test() -> ?assert(A == B).
% function_test_() -> ?_assert(A == B).

add_test_() ->
  [test_them_types(),
    test_them_values(),
    ?_assertError(badarith, 1/0)].

test_them_types() ->
  ?_assert(is_number(ops:add(1,2))).

test_them_values() ->
  [?_assertEqual(4, ops:add(2,2)),
   ?_assertEqual(3, ops:add(1,2)),
   ?_assertEqual(3, ops:add(1,1))].

%% > add_test_() のみが _test_() で終わるので、
%% > test_them_Something() の2つの関数はテストとみなされません。 事実、これらは add_test_() がテストを生成するときにのみ呼ばれます。

% 3> eunit:test(ops).
% ops_tests: new_add_test...*failed*
% in function ops_tests:'-new_add_test/0-fun-3-'/1 (ops_tests.erl, line 27)
% **error:{assertEqual_failed,[{module,ops_tests},
%                      {line,27},
%                      {expression,"ops : add ( 1 , 1 )"},
%                      {expected,3},
%                      {value,2}]}
%
%
% ops_tests:63: test_them_values...*failed*
% in function ops_tests:'-test_them_values/0-fun-4-'/1 (ops_tests.erl, line 63)
% **error:{assertEqual_failed,[{module,ops_tests},
%                      {line,63},
%                      {expression,"ops : add ( 1 , 1 )"},
%                      {expected,3},
%                      {value,2}]}
%
%
% =======================================================
%   Failed: 2.  Skipped: 0.  Passed: 5.
% error
