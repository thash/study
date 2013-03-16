%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 17. Callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server というOTPビヘイビア(?). 次のインターフェイスを持つ.
%%   1. init/1        ... サーバの状態を初期化して、サーバが依存する1度きりのタスクをすべて行う.
%%   2. handle_call/3 ... 同期メッセージと連携するときに使われます.
%%   3. handle_cast/2 ... Massage と State を引数に取り、非同期呼び出しを処理. 戻り値はreplyがないものに限る.
%%   4. handle_info/2 ... handle_castと似てる. 違いはコールバックが特殊なモノだけに対応していること.
%%   5. terminate/2   ... handle_*系が{stop,_,_} を返したときに呼ばれる.
%%   6. code_change/3 ... バージョン項をtaple第一要素に入れればアップグレード. 停止せずにコードを更新できる.

-module(kitty_gen_server).
-behaviour(gen_server).

%% ここでcでloadしようとすると
%% kitty_gen_server.erl:14: Warning: undefined callback function code_change/3 (behaviour 'gen_server') ... などと,
%% code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2
%% がないよと言われる. あらかじめ決められているのか.


%% behaviourの定義は簡単. moduleを作って, 以下のようなfunctionを作ればいい.
%% behaviour_info(callbacks) -> [{init,1}, {some_fun,0},{other,3}]
%% behaviour_info(_) -> undefined.


%% さて以下実装. 基本16/kitty_server2.erlと同じ. gen_serverをつかってるだけ.
%% つーかmy_server -> gen_serverに書き換えるだけじゃね.
start_link() -> gen_server:start_link(?MODULE, [], []).
order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).
return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).
close_shop(Pid) ->
  gen_server:call(Pid, terminate).

init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if Cats =:= [] ->
      %% [before] my_server:reply(From, make_cat(Name, Color, Description)),
      {reply, make_cat(Name, Color, Description), Cats};
      Cats;
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;

handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Cats}.

terminate(Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  exit(normal).

code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.

%%% Private functions
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.

