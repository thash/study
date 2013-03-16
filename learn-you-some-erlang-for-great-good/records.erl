%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 11. A Short Visit to Common Data Structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Record %%%

-module(records).
-compile(export_all).

-record(robot, {name,
    type=industrial,
    hobbies,
    details=[]}).

first_robot() ->
  #robot{name="Mechatron",
  type=handmade,
  details=["Moved by a small man inside"]}.

%% 5> records:first_robot().
%% {robot,"Mechatron",handmade,undefined,
%%        ["Moved by a small man inside"]}

%% cじゃなくてrrを使えば, タプルじゃなくてレコードとして表現してくれる.
%% あとrp()でtaple -> record変換

%% 1> rr(records).
%% [robot]
%% 2> records:first_robot().
%% #robot{name = "Mechatron",type = handmade,
%%        hobbies = undefined,
%%        details = ["Moved by a small man inside"]}
%%  出力差異に注意

%% アクセス -- # + . 記法
%  4> Crusher = #robot{name="Crusher", hobbies=["Crushing people","petting cats"]}.
%  5> Crusher#robot.hobbies.
%  ["Crushing people","petting cats"]

% NestedBot = #robot{details=#robot{name="erNest"}}.
% (NestedBot#robot.details)#robot.name.

%% Recordは関数のアタマでパターンマッチ/Guardとして使える

-record(user, {id, name, group, age}).

%% use pattern matching to filter
%% tapleとかlist的に名前を束縛しつつマッチングさせてるのがわかるね
admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is allowd!";
admin_panel(#user{name=Name}) ->
  Name ++ " is not allowd!".

%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
  allowed;
adult_section(_) ->
  forbidden.

%% updating records
%% Robot recordを引数にとっていろいろする
repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.

%% > レコードを直接書き換えたように見えます（ Rob#robot{Field=NewValue} ）が、これはすべて erlang:setelement/3 関数を呼び出すためにコンパイラを騙すためのものです。

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 11.3. KVS
%% proplistsに入ってる. シンプルな [{Key, Value}] のリスト.
%% proplists:get_value/2とか.
%% add/updateは単にcons(|)で [{NewKey, NewVal}|OldList] とする.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 11.4. 配列

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 11.5. Set (集合)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 11.6. 有向グラフ

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 11.7. Queue

