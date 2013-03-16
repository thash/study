-module(sockserv_serv).
-behavior(gen_server).

-record(state, {name, next, socket}).

%% わりといつもどおりのコールバック.
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  code_change/3, terminate/2]).

-define(TIME, 800).
-define(EXP, 50).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
   %% properly seeding the process
   <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
   random:send({A,B,C}),
   %% Because accepting a connection is a blocking function call, <= そうなん.
   %% we can not do it in here. Forward to the server loop!
   gen_server:cast(self(), accept),
   {ok, #state{socket=Socket}}.

 %% We never need you, handle_call! ひどい
 handle_call(_, _From, State) ->
   {noreply, State}.

%%% cryptoあたりについての解説.
%% > 他人の書いたコードを読むと、 random:seed/1 を now() の結果を使って呼んでいるのをしばしば見るかもしれません。 now() は単調な時間を返す（常に増加していて、決して二度と同じ値になることはない）ため、便利な関数です。 しかし、Erlangで使われている乱数アルゴリズムにとっては悪い乱数種です。 こうした理由から、 crypto:rand_bytes(12) （R14B03以上では crypto:strong_rand_bytes(12) ）を使って、12桁の安全に暗号化された乱数のバイト列を生成したほうがいいでしょう。 <<A:32, B:32, C:32>> とすることで、12バイトを3つの整数に変換することができます。

handle_cast(accept, S = #state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  sockserv_sup:start_socket(),
  send(AcceptSocket, "What's your character's name?", []),
  {noreply, S#state{socket=AcceptSocket, next=name}};

handle_cast(roll_stats, S = #state{socket=Socket}) ->
    Roll = pq_stats:initial_roll(),
    send(Socket,
         "Stats for your character:~n"
         "  Charisma: ~B~n"
         "  Constitution: ~B~n"
         "  Dexterity: ~B~n"
         "  Intelligence: ~B~n"
         "  Strength: ~B~n"
         "  Wisdom: ~B~n~n"
         "Do you agree to these? y/n~n",
         [Points || {_Name, Points} <- lists:sort(Roll)]),
    {noreply, S#state{next={stats, Roll}}};

%% The player has accepted the stats! Start the game!
handle_cast(stats_accepted, S = #state{name=Name, next={stats, Stats}}) ->
    processquest:start_player(Name, [{stats,Stats},{time,?TIME},
                                     {lvlexp, ?EXP}]),
    processquest:subscribe(Name, sockserv_pq_events, self()),
    {noreply, S#state{next=playing}};


%% Events coming in from process quest
%% We know this because all these events' tuples start with the
%% name of the player as part of the internal protocol defined for us
handle_cast(Event, S = #state{name=N, socket=Sock}) when element(1, Event) =:= N ->
    [case E of
        {wait, Time} -> timer:sleep(Time);
        IoList -> send(Sock, IoList, [])
     end || E <- sockserv_trans:to_str(Event)], % translate to a string
    {noreply, S}.

handle_info({tcp, _Socket, Str}, S = #state{next=name}) ->
    Name = line(Str),
    gen_server:cast(self(), roll_stats),
    {noreply, S#state{name=Name, next=stats}};

handle_info({tcp, Socket, Str}, S = #state{socket=Socket, next={stats, _}}) ->
    case line(Str) of
        "y" ->
            gen_server:cast(self(), stats_accepted);
        "n" ->
            gen_server:cast(self(), roll_stats);
        _ -> % ask again because we didn't get what we wanted
            send(Socket, "Answer with y (yes) or n (no)", [])
    end,
    {noreply, S};

handle_info({tcp, _Socket, "quit"++_}, S) ->
    processquest:stop_player(S#state.name),
    gen_tcp:close(S#state.socket),
    {stop, normal, S};

handle_info({tcp_closed, _}, S) ->
    {stop, normal, S};
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).

send(Socket, Str, Args) ->
    gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    inet:setopts(Socket, [{active, once}]),
    ok.

%% Let's get rid of the white space and ignore whatever's after.
%% makes it simpler to deal with telnet.
line(Str) ->
    hd(string:tokens(Str, "\r\n ")).
