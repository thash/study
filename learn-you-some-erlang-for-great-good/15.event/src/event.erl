%% まず設計をするよ. https://www.evernote.com/shard/s11/sh/44edeb25-a007-4044-abfb-3ae6c6bbff30/78aa25c86599bcac1f5d4bf600604708
%% * eventから書き始めるのは最も依存が少ないため.
%% * たいていのメッセージは{Pid, Ref, Message}の形でwrapされる.

%% event.erlの核は
% loop(State) ->
% receive
%     {Server, Ref, cancel} ->
%       ...
% after Delay ->
%     ...
% end.

-module(event).
-compile(export_all).
-record(state, {server, name="", to_go=0}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T*1000 ->
      %% こうすることで何年でもタイムアウトを待ち続ける.
      if Next =:= [] ->
          Server ! {done, S#state.name};
        Next =/= [] ->
          loop(S#state{to_go=Next})
      end
  end.

%% erlangのタイムアウトはmaxで50日.
%% タイムアウト時間が長すぎるときは49日で分割しましょう.
normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

%% lists:duplicateにについてちょっとしたtest.
% 9> lists:duplicate(3, 5).
% [5,5,5]
% 10> lists:duplicate(12 div 3 , 5).
% [5,5,5,5]
% 11> lists:duplicate(99 div 17, 5).
% [5,5,5,5,5]

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

%%% Event's innards
init(Server, EventName, DateTime) ->
  loop(#state{server=Server,
      name=EventName,
      to_go=time_to_go(DateTime)}).

cancel(Pid) ->
  %% Monitor in case the process is already dead
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.


time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
  calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0  -> ToGo;
    ToGo =< 0 -> 0
  end,
  normalize(Secs).
