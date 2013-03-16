%% supervisorはworkerを監視する.
%% > この章はこれまでで最も暴力的な章になります。親が子供を木に縛り付け、容赦無く殺すまで強制的に働かせることに時間を費やすのです。

%%% supervisorはinit/1のみ持ってるから使うのは簡単だけど, 戻り値は以下のように複雑.
%%% 一般系:
%%     {ok, {{RestartStrategy, MaxRestart, MaxTime},[ChildSpecs]}}.
%%% 具体例:
%% {ok, {{one_for_all, 5, 60},
%%       [{fake_id,
%%         {fake_mod, start_link, [SomeArg]},
%%         permanent,
%%         5000,
%%         worker,
%%         [fake_mod]},
%%        {other_id,
%%        {event_manager_mod, start_link, []},
%%         transient,
%%         infinity,
%%         worker,
%%         dynamic}]}}.

%%% workerが死んだ時の再起動戦略いろいろ. one_for_oneとか名前がついてたりする.

%% 使うbehaviourは"supervisor".

