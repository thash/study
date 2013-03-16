-module(dog_fsm).
-export([start/0, squirrel/1, pet/1]).

start() ->
  spawn(fun() -> bark() end).

squirrel(Pid) -> Pid ! squirrel.

pet(Pid) -> Pid ! pet.

bark() ->
  io:format("Dog says: BARK! BARK!~n"),
  receive
    pet ->
      wag_tail();
    _ ->
      io:format("Dog is confused~n"),
      bark()
  after 2000 ->
      %% 2000ms待って, タイムアウトで再度自身を呼び出す.
      %% だから何もしないでもうるさいわけだな. receive + after + 再帰でsleep loop的な動き.
      bark()
  end.

wag_tail() ->
  io:format("Dog wags its tail~n"),
  receive
    pet ->
      sit();
    _ ->
      io:format("Dog is confused~n"),
      wag_tail()
  after 30000 -> %% 尻尾振ってても30秒ほっとくとまた吠え出す. うぜぇｗｗｗ
      bark()
  end.

sit() ->
  io:format("Dog is sitting. Gooooood boy!~n"),
  receive
    squirrel ->
      bark();
    _ ->
      io:format("Dog is confused~n"),
      sit()
  end.

% 7> Pid = dog_fsm:start().
% Dog says: BARK! BARK!
% <0.50.0>
% Dog says: BARK! BARK!
% Dog says: BARK! BARK!
% Dog says: BARK! BARK!
% Dog says: BARK! BARK!
% %% うるせえｗｗｗｗｗｗ
% Dog says: BARK! BARK!
% Dog says: BARK! BARK!
% > dog_fsm:pet(Pid).
% Dog says: BARK! BARK!
% Dog says: BARK! BARK!
% > dog_fsm:pet(Pid).
% Dog wags its tail
%% おとなしくなった.

% 10> dog_fsm:squirrel(Pid).
% Dog is confused
% squirrel
% Dog wags its tail
% 11>
% 11> dog_fsm:pet(Pid).
% Dog is sitting. Gooooood boy!
% pet
