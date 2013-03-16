%%% pq_supersup is the ProcessQuest top-level supervisor.
%%% It sits over many pq_sup instances, allowing to have
%%% a truckload of different players running at once.
-module(pq_supersup).
-behavior(supervisor).
-export([start_link/0, start_player/2, stop_player/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok,
    {{simple_one_for_one, 1, 60000},
      [{sup,
          {pq_sup, start_link, []},
          permanent, infinity, supervisor, [pq_sup]}]}}.

start_player(Name, Info) ->
  supervisor:start_child(?MODULE, [Name, Info]).

stop_player(Name) ->
  supervisor:terminate_child(?MODULE, regis:whereis(Name)).
