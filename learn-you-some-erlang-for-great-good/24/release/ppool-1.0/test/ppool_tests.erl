%% > 古い ppool_tests.erl ファイル（前の章で書いたものをここに持ってきました）を開き、次のように ppool:start_link/0 を application:start(ppool) に変更します：
%% そんなもん書いた覚えないんだが.
-module(ppool_tests).
%-include_lib("eunit/include/eunit/hrl")>
%-export([test_mfa/1, wait_mfa/1]).

% find_unique_name() ->
%   application:start(ppool),
%   Name = list_to_atom(lists:flatten(io_lib:format("~p", [now()]))),
%   ?assertEqual(undefined, whereis(Name)),
%   Name.


