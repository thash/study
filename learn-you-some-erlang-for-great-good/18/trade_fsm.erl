-module(trade_fsm).
-behavior(gen_fsm).

%% public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
    make_offer/2, retract_offer/2, ready/1, cancel/1]).

%% gen_fsm callbacks (behaviorの"コールバック"は取り決めを指すらしいな)
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
    terminate/3, code_change/4,
    % custom state names
         idle/2, idle/3,
    idle_wait/2, idle_wait/3,
    negotiate/2, negotiate/3,
         wait/2,
        ready/2, ready/3]).

%% implement public API
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).
start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

%% ask for a begin session. Returns when/if the other accepts
trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% accept someone's trade offer.
accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% send an item on the table to be traded
make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% Cancel trade offer
retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Mention that you're ready for a trade. When the other
%% player also declares being ready, the trade is done
ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% Cancel the transaction.
cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).


%%% 次に, FSMをFSM関数に実装します %%%
%% Ask the other FSM's Pid for a trade session
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% Forward a client's offer
do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).
undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% ask the other side if he's ready to trade.
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

%% and reply.
not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).
am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

%% Acknowledge that the fsm is in a ready state.
%% syn-ackのackはacknowledgeだったのか...
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).
ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ack_commit).
do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).
notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).

%% さて次から gen_fsm コールバックの実装に入る.
%% gen_fsm behaviorインターフェイスに対応するパーツをはめていく.
-record(state, {name="",
    other,
    ownitems=[],
    otheritems=[],
    monitor,
    from}).

init(Name) ->
  {ok, idle, #state{name=Name}}.

%% Send players a notice. This could be messages to their clients
%% but for our purpose, outputting to the shell is enough.
notice(#state{name=N}, Str, Args) ->
  io:format("~s: "++Str++"~n", [N|Args]).

unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n",
    [self(), Msg, State]).

%% idle状態からスタートする.
idle({ask_negotiate, OtherPid}, S=#state{}) ->
  Ref = monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};

idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};

idle(Event, _From, Data) -> %% 他のプレーヤーに取引を持ちかけるとき.
  unexpected(Event, idle),
  {next_state, idle, Data}.


idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
%% idle_wait 相手が交渉を受け入れた後. Move to negotiate state
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting negotiation", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.


%% add/remove on/- item to/from an item list
add(Item, Items) -> [Item | Items].
remove(Item, Items) -> Items -- Item.


%% 以上を使ってアイテムの提示と撤回を実装できる.
negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
%% Own side retracting an item offer
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
%% other side offering an item
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
%% other side retracting an item offer
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "Other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};

%% さらにnegotiateはare_you_readyも扱わなければならない
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
  io:format("Other user ready to trade.~n"),
  notice(S,
    "Other user ready to transfer goods:~n"
    "You get ~p, The other side gets ~p",
    [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

%% negotiate/3
negotiate(ready, From, S = #state{other=OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "asking if ready, waiting", []),
  {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, S}.

%%% wait

wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "Other side cancelling offer of ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};

wait(are_you_ready, S=#state{}) ->
  am_ready(S#state.other),
  notice(S, "asked if ready, and I am. Waiting for same reply", []),
  {next_state, wait, S};

wait(not_yet, S=#state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};

wait('ready!', S=#state{}) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(S#state.from, ok),
  notice(S, "other side is ready. Moving to ready state", []),
  {next_state, ready, S};
%% Don't care about this!
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.


priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

ready(ack, S=#state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "asking for commit", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "ordering commit", []),
        ok = do_commit(S#state.other),
        notice(S, "committing...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
        %% abort! Either ready_commit or do_commit failed
        notice(S, "commit failed", []),
        {stop, {Class, Reason}, S}
    end;
  false ->
    {next_state, ready, S}
end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

%%% 続いてready/3. next_stateによって処理を変える
ready(ask_commit, _From, S) ->
  notice(S, "replying to ask_commit", []),
  {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

%% commit/1を実装. Rubyに慣れてると, arityが重要になるの新鮮.
commit(S = #state{}) ->
  io:format("Transaction completed for ~s. "
    "Items sent are:~n~p,~n received are:~n~p.~n"
    "This operation should have some atomic save "
    "in a database.~n",
    [S#state.name, S#state.ownitems, S#state.otheritems]).

% > 一般的に、真に安全なコミットを2者の関係性だけで行うことは出来ません。
% > 両プレーヤーがすべて正しく処理したかを判断すために、通常第3者が必要となります。
% > もしあなたが本当の commit 関数を書こうと思ったら、
% > その関数が両プレーヤーの代わりにその第3者に連絡を取って、彼らの代わりにデータベースに安全な書き込みをするか、あるいは全変更をロールバックします。


%%% プレーヤーが取引をキャンセルする時, および相手のFSMがクラッシュした時の処理
handle_event(cancel, _StateName, S=#state{}) ->
  notice(S, "received cancel event", []),
  {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
  notify_cancel(S#state.other),
  notice(S, "cancelling trade, sending cancel event", []),
  {stop, cancelled, ok, S};
%% cancel以外のinfoはunexpectedへ送る
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) ->
  notice(S, "Other side dead", []),
  {stop, {other_down, Reason}, S};
%% 'DOWN'以外のinfoはunexpectedへ送る
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.
terminate(normal, ready, S=#state{}) ->
  notice(S, "FSM leaving", []);
terminate(_Reason, _StateName, _StateData) ->
  ok.


%% > もし混乱してしまったとしたら、こう自問してみてください。
%% > * 「あなたは、プロセスがどの状態にあるかでイベントの扱われ方がどう異なるか理解できましたか？」
%%     * => 状態ごとに別個の関数を定義するため同じイベントも異なる扱いになる.
%% > * 「あなたはどうやってある状態から別の状態への遷移できるか理解しましたか？」
%%     * => 別の状態を表す関数を実行してやると良い. その先でreceiveして待つ.
%%     * => gen_fsmを使う場合, {next_state, s, _} とすることで状態sに遷移する.
%% > * 「いつ send_event/2 と sync_send_event/2-3 を使って、逆にいつ send_all_state_event/2 と sync_send_all_state_event/3 を使うかわかりますか？」
%%     * => syncが入る2つは同期イベント. 相手と通信して反応を待つ.
%%     * => allが入る2つはグローバルイベント. どんな状態にあっても同じ結果を引き起こす.
%% > もしこれらの質問に「はい」と答えられるなら、 gen_fsm が何か理解したと言えましょう。

