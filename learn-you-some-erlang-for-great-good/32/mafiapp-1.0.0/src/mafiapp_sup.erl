-module(mafiapp_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Tables) ->
  supervisor:start_link(?MODULE, Tables).

init(Tables) ->
  mnesia:wait_for_tables(Tables, 5000),
  {ok, {{one_for_one, 1, 1}, []}}.
