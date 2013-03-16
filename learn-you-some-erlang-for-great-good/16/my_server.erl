-module(my_server).
-compile([start/2, start_link/2, call/2, cast/2, reply/2]).

%%% Public API
start(Module, InitialState) ->
  spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
  spawn_link(fun() -> init(Module, InitialState) end). %% init/0 という呼び方と fun() -> init() endってどう違うんだっけ.

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
      erlang:error(timeout)
  end.

cast(Pid, Msg) ->
  Pid ! {async, Msg},
  ok.

reply({Pid, Ref}, Reply) ->
  Pid ! {Ref, Reply}.

%% Private stuff
init(Module, InitialState) ->
  loop(Module, Module:init(InitialState)).

%% パターンマッチをloopから切り離さねばならない. handleに渡す.
loop(Module, State) ->
  receive
    %% asyncとsyncを並列に扱えるのは美しい
    {async, Msg} ->
      loop(Module, Module:handle_cast(Msg, State));
    {sync, Pid, Ref, Msg} ->
      loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
      %% {Pid, Ref} とタプルに入れて抽象化する. タプル形式をそのまま扱うreplyを作ってそいつに任せる.
  end.

