-module(m8ball).
-behavior(application).
-export([start/2, stop/1]).
-export([ask/1]).

%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%
start(normal, []) ->
  m8ball_sup:start_link();
%% 分散OTPアプリケーションにするには, takeoverの受け取り口を用意
start({takeover, _OtherNode}, []) ->
  m8ball_sup:start_link().

stop(_State) ->
  ok.

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
ask(Question) ->
  m8ball_server:ask(Question).

%% 谺｡縺ｫ.app繝輔ぃ繧､繝ｫ...縺薙ｌ縺ｯebin莉･荳九↓鄂ｮ縺上ｈ
