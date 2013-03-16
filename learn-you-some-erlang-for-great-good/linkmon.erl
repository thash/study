%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 14. エラーとプロセス
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 14.1. リンク

-module(linkmon).
-compile(export_all).

myproc() ->
  timer:sleep(5000),
  exit(reason).

%% shellがreasonでクラッシュする.

% 1> c(linkmon).
% 2> spawn(fun linkmon:myproc/0).
% <0.38.0>
% 3> link(spawn(fun linkmon:myproc/0)).
% true
%% ... ここで5秒待つ...
%  ** exception error: reason

chain(0) ->
  receive
    _ -> ok
  after 2000 ->
      exit("chain dies here")
  end;
chain(N) ->
  Pid = spawn(fun() -> chain(N-1) end),
  link(Pid),
  receive
    _ -> ok
  end.

% 5> link(spawn(linkmon, chain, [3])).
% true
%  ** exception error: "chain dies here"

%% > [shell] == [3] == [2] == [1] == [0]
%% > [shell] == [3] == [2] == [1] == *dead*
%% > [shell] == [3] == [2] == *dead*
%% > [shell] == [3] == *dead*
%% > [shell] == *dead*
%% > *dead, error message shown*
%% > [shell] <-- restarted


%%% process_flag(trap_exit, true) を使うと...
% 6> process_flag(trap_exit, true).
% false
% 7> spawn_link(fun() -> linkmon:chain(3) end).
% <0.56.0>
% 8> receive X -> X end.
% {'EXIT',<0.56.0>,"chain dies here"}

%% exitした事実を他のなんかに渡して自分は生き残る
%% これを使えば, 落ちた時そいつを再起動するだけのプロセスを簡単に作れる.


%%% 14.3. モニター ... 特殊なリンク

%% 特殊性(1). 一方向である
%% 特殊性(2). スタックできる

% 1> erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
% #Ref<0.0.0.27>
% 2> flush().
% Shell got {'DOWN',#Ref<0.0.0.27>,process,<0.33.0>,normal}


%%% 14.4. プロセスに名前をつける
start_critic() ->
  spawn(?MODULE, critic, []).
judge(Pid, Band, Album) ->
  Pid ! {self(), {Band, Album}},
  receive
    {Pid, Criticism} -> Criticism
  after 2000 ->
      timeout
  end.

critic() ->
  receive
    {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {self(), "They are great!"};
    {From, {"System of a Downtime", "Memoize"}} ->
      From ! {self(), "They're not Johnny Crash but they're good."};
    {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {self(), "Simply incredible."};
    {From, {_Band, _Album}} ->
      From ! {self(), "They are terrible!"}
  end,
  critic().

%% プロセスが落ちた時再起動するsupervisor
start_critic2() ->
  spawn(?MODULE, restarter, []).
restarter() ->
  process_flag(trap_exit, true), %% 色がついた所を見るとtrap_exitは予約語的なソレか. spawn_linkも.
  Pid = spawn_link(?MODULE, cirtic, []), %% 起動しつつその後を見守る.
  register(critic, Pid), %% ここでプロセスに"critic"という名前を付けられる.
  receive
    {'Exit', Pid, normal} -> % not a crash
      ok;
    {'Exit', Pid, shutdown} -> % manual termination, not a crash
      ok;
    {'Exit', Pid, _} ->
      restarter()
  end.
%% 同じ名前で複数起動したらどうするんだ? VMが適切に割り振ってくれるとかそんな素敵なことあるのか?
%% => whereisがそれっぽいことをやってる. "見つけて"る.

%% ともかくjudgeの引数からPidをなくそう. 送り先は常にciritic.
judge2(Band, Album) ->
  Ref = make_ref(), %% これまた組み込み? メッセージに一意な値を与えるmake_ref. どうやって一意を保証してるんだろ.
  critic ! {self(), Ref, {Band, Album}},
  receive
    {Ref, Criticism} -> Criticism
  after 2000 ->
      timeout
  end.

%% ここまで考えると, いろいろと競合状態が起き得ることが予測できる.
%% 競合状態回避のためRefを使う.
critic2() ->
  %% 受け取る値にRefが入り, Fromに返すのはselfじゃなくRefをそのまま引き渡す.
  receive
    {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {Ref, "They are great!"};
    {From, Ref, {"System of a Downtime", "Memoize"}} ->
      From ! {Ref, "They're not Johnny Crash but they're good."};
    {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {Ref, "Simply incredible."};
    {From, Ref, {_Band, _Album}} ->
      From ! {Ref, "They are terrible!"}
  end,
  critic2(). %% ここテキストだとcriticだけど, critic2では?

