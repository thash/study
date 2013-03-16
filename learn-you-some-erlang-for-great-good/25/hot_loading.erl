%% live code upgradeは基本的に危険があるもの.

%% live code upgrade機能付きの基本的な構成はこんな感じ.
loop(N) ->
  receive
    some_standard_message -> N+1;
    other_message -> N-1;
    {get_count, Pid} ->
      Pid ! N,
      loop(N);
    % (1). update -> ?MODULE:loop(N); -- loopの引数自体が変わるときに対応できない.
    update -> ?MODULE:code_change(N); %% こうやって, code_change/1から新しいバージョンのloopを呼び出す.
end.

%% しかし次のようなコードだと破綻する.
loop(Mod, State) ->
  receive
    {call, From, Msg} ->
      {reply, Reply, NewState} = Mod:handle_call(Msg, State),
      From ! Reply,
      loop(Mod, NewState);
    update ->
      {ok, NewState} = Mod:code_change(State),
      loop(Mod, NewState)
  end.

% > Mod を更新して、新しいバージョンをロードしたい場合、この実装ではそれを安全に行う方法がありません。
% > Mod:handle_call(Msg, State) はすでに完全修飾で、 {call, From, Msg} という形式のメッセージをコードのリロードと更新メッセージの処理の間に受け取る事は十分ありえます。
% > この場合、モジュールの更新は制御不能な状態となり、クラッシュします。

%% このへんをうまくやってくれるのがOTPビヘイビア深層部にある sys moduleとrelease_handler (SASL = System Architecture Support Libraries).
%% relup(リリース全体の更新方法)

