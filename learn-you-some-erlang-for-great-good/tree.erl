-module(tree).
-export([empty/0,insert/3,lookup/2]).

empty() -> {node,'nil'}.

%% 大きさを判定してどんどん受け渡していく.
insert(Key, Val, {node, 'nil'}) ->
  {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
  {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
  {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
  {node, {Key, Val, Smaller, Larger}}.


lookup(_, {node, 'nil'}) ->
  undefined; %% こんなんあったのか
lookup(Key, {node, {Key, Val, _, _}}) ->
  {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
  lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
  lookup(Key, Larger).

%    30> T1 = tree:insert("Jim Woodland", "jim.woodland@gmail.com", tree:empty()).
%    {node,{"Jim Woodland","jim.woodland@gmail.com",
%           {node,nil},
%           {node,nil}}}
%    31> T2 = tree:insert("Mark Anderson", "i.am.a@hotmail.com", T1).
%    {node,{"Jim Woodland","jim.woodland@gmail.com",
%           {node,nil},
%           {node,{"Mark Anderson","i.am.a@hotmail.com",
%                  {node,nil},
%                  {node,nil}}}}}
%    32> Addresses = tree:insert("Anita Bath", "abath@someuni.edu", tre
%    e:insert("Kevin Robert", "myfairy@yahoo.com", tree:insert("Wilson
%    Longbrow", "longwil@gmail.com", T2))).
%    {node,{"Jim Woodland","jim.woodland@gmail.com",
%           {node,{"Anita Bath","abath@someuni.edu",
%                  {node,nil},
%                  {node,nil}}},
%           {node,{"Mark Anderson","i.am.a@hotmail.com",
%                  {node,{"Kevin Robert","myfairy@yahoo.com",
%                         {node,nil},
%                         {node,nil}}},
%                  {node,{"Wilson Longbrow","longwil@gmail.com",
%                         {node,nil},
%                         {node,nil}}}}}}}
%    33> tree:lookup("Anita Bath", Addresses).
%    {ok,"abath@someuni.edu"}
