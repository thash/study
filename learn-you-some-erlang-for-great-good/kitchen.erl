%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 13. マルチプロセスについてもっと
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(kitchen).
-compile(export_all).

fridge1() ->
  receive
    % _から始まる変数は名前つけるけど覚えない = 上書きを拒否しない, だっけ
    {From, {store, _Food}} ->
      From ! {self(), ok},
      fridge1();
    {From, {take, _Food}} ->
      %% uh... <- なんやねん
      From ! {self(), not_found},
      fridge1();
    terminate ->
      ok
  end.

%% > 再帰のおかげで、プロセスの状態は関数のパラメータの中にすべて保持出来ます。 冷蔵庫プロセスの場合は、リストの形ですべての食物を保存しておき、誰かが何か食べたいと思った時にそのリストを見る、というような方法が考えられます:

fridge2(FoodList) ->
  receive
    %% 今回はFoodを覚えようとしている
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge2([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food,FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge2(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge2(FoodList)
      end;
    terminate ->
      ok
  end.

% 1> c(kitchen).
% {ok,kitchen}
% 2> Pid = spawn(kitchen, fridge2, [[baking_soda]]).
% <0.38.0>
% 3> Pid ! {self(), {store, milk}}.
% {<0.31.0>,{store,milk}}
% 5> flush().
% Shell got {<0.38.0>,ok} %% 保存された
% ok
% 6> Pid ! {self(), {store, bacon}}.
% {<0.31.0>,{store,bacon}}
% 7> Pid ! {self(), {take, bacon}}.
% {<0.31.0>,{take,bacon}}
% 8> flush().
% Shell got {<0.38.0>,ok}
% Shell got {<0.38.0>,{ok,bacon}} %% baconをtake.

%% ないものをtakeしようとしてみる
% 9> Pid ! {self(), {take, udon}}.
% {<0.31.0>,{take,udon}}
% 10> flush().
% Shell got {<0.38.0>,not_found}


%% store, takeを関数にして抽象化しよう
store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  end.
take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

% 13> Pid = spawn(kitchen, fridge2, [[baking_soda]]).
% <0.54.0>
% 14> kitchen:store(Pid, water).
% ok
% 15> kitchen:take(Pid, water).
% {ok,water}

%% ユーザがプロセスを機動することすら必要なくしてしまおう.
%% Pid = kitchen:start([rhubarb, dog, hotdog]). と使う.
start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

%% ?MODULE -> ハテナ+MODULEで現在のモジュール名を返すマクロになる.

%%% タイムアウト処理 %%%
%% > 一般的に、非同期処理を行うときは（Erlangではメッセージパッシングで行われます）一定時間を過ぎてもデータを取得できる気配がなければ、諦めるようにする方法が必要です。
%% Erlangは言語組込で "after Delay" という仕組みを持ってる. receiveのところで使う.

store2(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg %% あれ, ここ;にしなくてえんや.
  after 3000 -> %% 3000ms = 3秒.
      timeout
  end.
take2(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
      timeout
  end.

%% spawnするとPidを返すからそれを使いまわしてたけど,
%% その代わりにpid(0,250,0)などとすると好きなpidにメッセージを送れる.

% 17> kitchen:take2(pid(0,250,0), dog).
% timeout
% 18> kitchen:take(pid(0,250,0), dog).
% => 固まる.


