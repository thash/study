%% いちばん素直に実装したTCPサーバ.
-module(naive_tcp).
-compile(export_all).

start_server(Port) ->
  Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
        spawn(fun() -> acceptor(Listen) end),
        timer:sleep(infinity)
    end),
  {ok, Pid}.

acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handle(Socket)
  end.


%% 動かしてみる
% 1> c(naive_tcp).
% {ok,naive_tcp}
% 2>
% 2> naive_tcp:start_server(9393).
% {ok,<0.38.0>}

% neon $ telnet localhost 9393
% Trying ::1...
% telnet: connect to address ::1: Connection refused
% Trying 127.0.0.1...
% Connected to localhost.
% Escape character is '^]'.
% hoge
% hoge
% baka
% baka
% quit
% Connection closed by foreign host.

%% 動くことは動く. が, 大量にアクセスが来たら順番にacceptしていくことになる.
%% そこでsockserv_supを導入, supervisorとして使う. => sockserv_sup.erl
