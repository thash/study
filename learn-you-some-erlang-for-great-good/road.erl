% 1> {ok,Binary} = file:read_file("road.txt").
% {ok,<<"50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0\n">>}
% 2> Binary.
% <<"50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0\n">>
% 3> S = string:tokens(binary_to_list(Binary), "\r\n\t").
% ["50","10","30","5","90","20","40","2","25","10","8","0"]
% 4> [list_to_integer(X) || X <- 5].
% ** exception error: bad generator 5
% 5> [list_to_integer(X) || X <- S].
% [50,10,30,5,90,20,40,2,25,10,8,0]

-module(road).
-compile(export_all).

main([FileName]) ->
  {ok,Bin} = file:read_file(FileName),
  Map = parse_map(Bin),
  io:format("~p~n",[optimal_path(Map)]),
  erlang:halt().

%% 型変換してもっかい自身を呼び出すという使い方.
parse_map(Bin) when is_binary(Bin) ->
  parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
  Values = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")],
  group_vals(Values, []).

%% 地図をparseして{A,B,X}の形にする
group_vals([], Acc) ->
  lists:reverse(Acc);
group_vals([A,B,X|Rest], Acc) ->
  %% 要するに前3個取ってtapleにまとめてるだけなんだけど綺麗に書けるな
  group_vals(Rest, [{A,B,X}|Acc]).


%% 解答ロジックの本体はここ.
%% {DistanceSum, PathList} という形式.
shortest_step({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->
  OptA1 = {DistA + A, [{a,A}|PathA]},
  OptA2 = {DistB + B + X, [{x,X}, {b,B}|PathB]},
  OptB1 = {DistB + B, [{b,B}|PathB]},
  OptB2 = {DistA + A + X, [{x,X}, {a,A}|PathA]},
  {erlang:min(OptA1, OptA2), erlang:min(OptB1,OptB2)}.

optimal_path(Map) ->
  {A,B} = lists:foldl(fun shortest_step/2, {{0,[]}, {0,[]}}, Map),
  {_Dist,Path} = if hd(element(2,A)) =/= {x,0} -> A;
    hd(element(2,B)) =/= {x,0} -> B
  end,
  lists:reverse(Path).

%    4> road:main(["road.txt"]).
%    [{b,10},{x,30},{a,5},{x,20},{b,2},{b,8}]

