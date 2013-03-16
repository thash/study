-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/1, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
%% > 2つ目の -export() を追加したことに注意して下さい.
%% > これらはちゃんと動作するために my_server が呼び出す必要がある関数です.

-record(cat, {name, color=green, description}).

%%% Client Api
start_link() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  my_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
  my_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
  my_server:call(Pid, terminate).

%%% Server funcitons
init([]) -> [].

handle_call({order, Name, Color, Description}, From, Cats) ->
  if Cats =:= [] ->
      my_server:reply(From, make_cat(Name, Color, Description)),
      Cats;
    Cats =/= [] ->
      my_server:reply(From, hd(Cats)),
      tl(Cats)
  end;

handle_call(terminate, From, Cats) ->
  my_server:reply(From, ok),
  terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
  [Cat|Cats].

%%% Private functions
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  exit(normal).

%% > ok を前に作った terminate/1 内の exit(normal) に置き換えることを忘れないようにして下さい。
%% > そうしないと、サーバは動き続けてしまいます。
