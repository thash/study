-module(cat_fsm).
-export([start/0, event/2]).

start() ->
  %% 復習: spawn/1は関数を受け取り, <0.33.0>などのプロセス識別子を返す.
  %%        通信するときにPid ! hogeと使われるので保持するのは大事.
  spawn(fun() -> dont_give_crap() end).

event(Pid, Event) ->
  %% 復習: make_refは"参照"を作る. これは"自分自身のアドレス"で#Ref<0.0.0.29> と見える.
  Ref = make_ref(), % won't care for monitors here
  Pid ! {self(), Ref, Event},

  %% eventレイヤーのreceiveでは通信を担当しているイメージ.
  %% eventはconsoleから呼ばれるのでコンソールにレスを返す役割.
  %% SICP4章で言うdriver-loop中の1関数か.
  receive
    {Ref, Msg} -> {ok, Msg}
  after 5000 ->
    {error, timeout}
  end.

dont_give_crap() ->
  receive
    {Pid, Ref, _Msg} -> Pid ! {Ref, meh};
    _ -> ok
  end,
  io:format("Switching to 'dont_give_crap' state~n"),
  dont_give_crap().

