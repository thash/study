%% http://memerelics.tumblr.com/post/36799339335
-module(hotload).
-export([server/1, upgrade/1]).

server(State) ->
  receive
    update ->
      NewState = ?MODULE:upgrade(State),
      ?MODULE:server(NewState); %% loop in the new version of the module
    _ ->
      %% do something
      server(State) %% stay in the same version
  end.

upgrade(OldState) ->
  %% transform and return the state here.
  io:format("hoge~p", [OldState]).
