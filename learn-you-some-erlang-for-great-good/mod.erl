-module(mod).
-export([funtest/0, funtest/1, funtest/2]).

%% 同名でも引数の数を変えればok
%% 与えた引数は関数内部で必ず使わないといけない(エラーではない?が, syntax警告が出た)
funtest() -> 1.
funtest(N) -> N * 2.
funtest(N, M) -> N + M.


%% c(filename) はcompileの略. beamが出現する.
%% beamというのはErlang VMの名前. かつては他のVMもあったが現存しない.

%% compile flag.
%% > compile:file(useless, [debug_info, export_all]).

%% > modulename:module_info().


