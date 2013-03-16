%%% The supervisor in charge of all the socket acceptors.
-module(sockserv_sup).
-behavior(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Port} = application:get_env(port),
  %% Set the socket into {active_once} mode.
  %% See sockserv_serv comments for more details <- どこやねん
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},
      [{socket,
          {sockserv_serv, start_link, [ListenSocket]}, % pass the socket!だそうな
          temporary, 1000, worker, [sockserv_serv]}
      ]}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

%%% じゃあとりあえず20個listenerを立てましょう

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.

%% んでつぎ sockserv_serv.erl の実装ね.

