-module(curling).
-export([start_link/2, set_teams/3, add_point/3, next_round/1]).
-export([join_feed/2, leave_feed/2]).
-export([game_info/1]).

%% 入ってくるイベントをすべて監視して適切な関数を呼び出す = イベントマネージャ.

start_link(TeamA, TeamB) ->
  {ok, Pid} = gen_event:start_link(),
  %% The scoarboard will always be there
  gen_event:add_handler(Pid, curling_scoreboard, []),
  %% Start the stats accumulator
  gen_event:add_handler(Pid, curling_accumulator, []),
  set_teams(Pid, TeamA, TeamB),
  {ok, Pid}.

set_teams(Pid, TeamA, TeamB) ->
  gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

add_point(Pid, Team, N) ->
  gen_event:notify(Pid, {add_point, Team, N}).

next_round(Pid) ->
  gen_event:notify(Pid, next_round).

game_info(Pid) ->
  gen_event:call(Pid, curling_accumulator, game_data).

%% > イベントマネージャが終了するときには、次のどちらかになります:
%% > * 終了を捕捉していないため {gen_event_EXIT, HandlerId, Reason} メッセージを受信してからクラッシュする
%% > * {gen_event_EXIT, HandlerId, Reason} メッセージを受信してから、余計あるいは混乱の元である、通常の 'EXIT' メッセージを受信する。

