-module(raw_tcp_spammer).
-compile(export_all).

-record(state, {}).

start() ->
    start(1, 9999).

start(Num, LPort) ->
    case gen_tcp:listen(LPort, [{active, false}, {packet,2}]) of
        {ok, ListenSock} ->
            start_servers(Num, ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error, Reason} ->
            {error, Reason}
    end.

start_servers(0,_) ->
    ok;
start_servers(Num, LS) ->
    register(?MODULE, spawn(?MODULE, server, [LS])),
    start_servers(Num-1, LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok, S} ->
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.


loop(S) ->
    inet:setopts(S, [{active, once}]),
    receive
        { spot, Text } ->
            gen_tcp:send(S, io_lib:format("~p~n", [Text])),
            loop(S);
        Other ->
            io:format("Random stuff: ~w~n", [Other]),
            loop(S)
    end.
