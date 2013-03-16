-module(regis_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%% テスト対象 %%%%%%%%%%%%%%%%%%%%%%%%%
%%% 25/processquest/apps/regis-1.0.0/src/regis_server.erl %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% > 私はかなりのTDDファンなので、確認しておきたいすべての機能のリストを書くことから始めます：
% >
% >   * インターフェースをErlangのデフォルトのプロセスレジストリに似せる
% >   * Pidを追跡することなく連絡を取れるように、サーバは登録名を持つ
% >   * プロセスは私たちのサービス経由で登録され、その名前によって連絡を取ることができる
% >   * すべての登録済みプロセスのリストが取得できる
% >   * どのプロセスによっても登録されていない名前は、その名前を使った呼び出しをクラッシュさせるために ‘undefined’ というアトムを返す。（通常のErlangレジストリと同様です）
% >   * プロセスは2つの名前を持てない
% >   * 2つのプロセスは同じ名前を共有できない
% >   * 一度登録されたプロセスも、呼び出しの間に登録が外されたら、再度登録ができる
% >   * プロセスの登録を外す処理は決してクラッシュしない
% >   * 登録済みプロセスがクラッシュした場合は、その名前を登録から外す
% >
% > これが準拠すべきリストです。 私がやったように、リストの項目を一つ一つ実装していくのではなく、各仕様を各々テストに変換していきました。

%% whereisについて
%% http://erlang.org/doc/man/erlang.html#whereis-1
%% > whereis(RegName) -> pid() | port() | undefined
%% >   Returns the pid or port identifier with the registered name RegName. Returns undefined if the name is not registered.
%% >   > whereis(db).
%% >   <0.43.0>


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
  {"The server can be started, stopped and has a registered name",
    ?setup(fun is_registered/1)}.

register_test_() ->
  [{"A process can be registered and contacted",
      ?setup(fun register_contact/1)},
    {"A list of registered processes can be obtained",
      ?setup(fun registered_list/1)},
    {"An undefined name should return 'undefined' to crash calls",
      ?setup(fun noregister/1)},
    {"A process can not have two names",
      ?setup(fun two_names_one_pid/1)},
    {"Two processes cannot share the same name",
      ?setup(fun two_pids_one_name/1)}].

unregister_test_() ->
  [{"A process that was registered can be registered again iff it was "
      "unregistered between both calls",
      ?setup(fun re_un_register/1)},
    {"Unregistering never crashes",
      ?setup(fun unregister_nocrash/1)},
    {"A crash unregisters a process",
      ?setup(fun crash_unregisters/1)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  {ok, Pid} = regis_server:start_link(),
  Pid.

stop(_) ->
  regis_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_registered(Pid) ->
  [?_assert(erlang:is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(regis_server))].

register_contact(_) ->
  Pid = spawn_link(fun() -> callback(regcontact) end),
  timer:sleep(15),
  Ref = make_ref(),
  WherePid = regis_server:whereis(regcontact),
  regis_server:whereis(regcontact) ! {self(), Ref, hi},
  Rec = receive
    {Ref, hi} -> true
  after 2000 -> false
  end,
  [?_assertEqual(Pid, WherePid),
    ?_assert(Rec)].

registered_list(_) ->
  L1 = regis_server:get_names(),
  Pids = [spawn(fun() -> callback(N) end) || N <- lists:seq(1,15)],
  timer:sleep(200),
  L2 = regis_server:get_names(),
  [exit(Pid, kill) || Pid <- Pids],
  [?_assertEqual([], L1),
    ?_assertEqual(lists:sort(lists:seq(1,15)), lists:sort(L2))].

noregister(_) ->
  [?_assertError(badarg, regis_server:whereis(make_ref()) ! hi),
    ?_assertEqual(undefined, regis_server:whereis(make_ref()))].

two_names_one_pid(_) ->
  ok = regis_server:register(make_ref(), self()),
  Res = regis_server:register(make_ref(), self()),
  [?_assertEqual({error, already_named}, Res)].


 two_pids_one_name(_) ->
     Pid = spawn(fun() -> callback(myname) end),
     timer:sleep(15),
     Res = regis_server:register(myname, self()),
     exit(Pid, kill),
     [?_assertEqual({error, name_taken}, Res)].

re_un_register(_) ->
    Ref = make_ref(),
    L = [regis_server:register(Ref, self()),
         regis_server:register(make_ref(), self()),
         regis_server:unregister(Ref),
         regis_server:register(make_ref(), self())],
    [?_assertEqual([ok, {error, already_named}, ok, ok], L)].

unregister_nocrash(_) ->
    ?_assertEqual(ok, regis_server:unregister(make_ref())).

crash_unregisters(_) ->
    Ref = make_ref(),
    Pid = spawn(fun() -> callback(Ref) end),
    timer:sleep(150),
    Pid = regis_server:whereis(Ref),
    exit(Pid, kill),
    ?_assertEqual(undefined, regis_server:whereis(Ref)).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
callback(Name) ->
  ok = regis_server:register(Name, self()),
  receive
    {From, Ref, Msg} -> From ! {Ref, Msg}
  end.

