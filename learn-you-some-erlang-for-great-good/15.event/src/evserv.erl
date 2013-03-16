-module(evserv).
-compile(export_all).

-record(state, {events,    %% list of event{} records
                clients}). %% list of Pids

-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

%%% User Interface
start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

init() ->
  %% Loading events from a static file could be done here.
  %% orddict = Ordered Dictionary? だいたいあってる
  %% > Key-Value Dictionary as Ordered List.
  %% [{Key, Value}, {Key, Value}...] という形.
  loop(#state{events=orddict:new(),
              clients=orddict:new()}).

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

add_event2(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, {error, Reason}} -> erlang:error(Reason);
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        M = {done, _Name, _Description} ->
            [M | listen(0)]
    after Delay*1000 ->
        []
    end.

%% The server itself
%% 15.2 で定義した"プロトコル" をパターンマッチで分岐させて処理.
loop(S = #state{}) ->
  %% いろんなeventを受け取って処理する
  receive
    %% > 最初のメッセージはサブスクライブに関するものです。
    %% > イベントが終わったときに通知しなければいけないので、すべてのサブスクライバのリストを保持したいです。
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      case valid_datetime(TimeOut) of
        %% > 新しいイベントプロセスをspawnして、
        %% > 呼び出し元に確認メッセージを送る前に、そのデータをイベントサーバの state に保存します
        true ->
          EventPid = event:start_link(Name, TimeOut),
          NewEvents = orddict:store(Name,
                                   #event{name=Name,
                                     description=Description,
                                     pid=EventPid,
                                     timeout=TimeOut},
                                   S#state.events),
          Pid ! {MsgRef, ok},
          %% 受け取った状態のeventsを変更して渡す
          loop(S#state{events=NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          %% 受け取った状態をそのまま渡す
          loop(S)
      end;
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
                    {ok, E} ->
                      event:cancel(E#event.pid), %% event.erl で定義したもの
                      orddict:erase(Name, S#state.events);
                    error ->
                      S#state.events
                    end,
      Pid ! {MsgRef, ok},
      loop(S#state{events=Events});
    %% > サーバとイベント自身の間でやり取りされる物についての処理を書いていきましょう。
    %% > 扱うメッセージとしては2つあります:
    %% >   イベントのキャンセル（もう実装されました）とイベントのタイムアウトです。
    {done, Name} ->
      E = orddict:fetch(Name, S#state.events),
      send_to_clients({done, E#event.name, E#event.description},
      S#state.clients),
      NewEvents = orddict:erase(Name, S#state.events),
      loop(S#state{events=NewEvents});
    shutdown ->
      exit(shutdown);
    %% > 'DOWN' メッセージの動作もかなり単純です。
    %% > これはクライアントが死んだことを意味しているので、 state 内のクライアントのリストから削除します。
    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
    %% ホットコードローディング -- ETS Tableを管理するコードサーバがやってくれる.
    code_change ->
      ?MODULE:loop(S);
    {Pid, debug} -> %% used as a hack to let me do some unit testing
      Pid ! S,
      loop(S);
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)
  end.


%%% Internal Functions
send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

valid_datetime({Date,Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> %% not in {{D,M,Y},{H,Min,S}} format
      false
  end;
valid_datetime(_) ->
  false.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
