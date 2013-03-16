-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl"). %% ct = common test
-export([all/0]).
-export([test1/1, test2/1]).

all() -> [test1,test2].

%% Configには, (今は使われてないけど)テストケースに必要な初期状態が渡される.
test1(_Config) ->
  1 = 1.

test2(_Config) ->
  A = 0,
  1/A.

% $ ct_run -suite basic_SUITE
% こんなのが生成される.
% drwxr-xr-x  12 hash  staff   408B 12 11 15:12 ct_run.ct@neon.2012-12-11_15.11.58/
% -rw-r--r--   1 hash  staff   329B 12 11 15:13 basic_SUITE.erl
% -rw-r--r--   1 hash  staff    82B 12 11 15:13 variables-ct@neon
% -rw-r--r--   1 hash  staff    16K 12 11 15:13 jquery.tablesorter.min.js
% -rw-r--r--   1 hash  staff    71K 12 11 15:13 jquery-latest.js
% -rw-r--r--   1 hash  staff   4.1K 12 11 15:13 ct_default.css
% -rw-r--r--   1 hash  staff   4.4K 12 11 15:14 index.html
% drwxr-xr-x  11 hash  staff   374B 12 11 15:14 ct_run.ct@neon.2012-12-11_15.13.59/
% -rw-r--r--   1 hash  staff   1.3K 12 11 15:14 basic_SUITE.beam
% -rw-r--r--   1 hash  staff   4.5K 12 11 15:14 all_runs.html
