%% polynya-cluster
%%% @author Michael Clarke, M0PRL <clarkema -at- clarkema.org>
%%% @copyright 2012 Michael Clarke
%%% @end
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License Version 3, as
%%% published by the Free Software Foundation.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% ----------------------------------------------------------------------
%%% @doc
%%% 
%%% @end
%%% ----------------------------------------------------------------------
-module(node).
-behaviour(gen_server).
-include("spot.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {localNode, remoteNode, host, port, link, pending}).

%% =======================================================================
%% Behaviour Exports
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% =======================================================================
%% Module Exports
-export([start_link/1]).

%% =======================================================================
%% Behaviour API

%% @private
init({peer, LocalNode, RemoteNode, _Password, Host, Port}) ->
    case gen_tcp:connect(Host, Port, [list, inet, {packet, raw}], 3000) of
        {ok, _Socket} ->
            {ok, #state{localNode=LocalNode,
                        remoteNode=RemoteNode,
                        host=Host,
                        port=Port,
                        link=disconnected,
                        pending=""}};
        {error, Reason} ->
            {stop, {failed_connect, Reason}}
    end.

%% @private
handle_call({disconnect}, _From, State) ->
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({tcp, Socket, Data}, #state{link=disconnected} = State) ->
    {noreply, process_connection(Socket, string:tokens(Data, "\r\n"), State)};
handle_info({tcp, Socket, Data}, #state{link=connected, pending=Pending} = State) ->
    case string:right(Data, 2) of
        "\r\n" ->
            %% We've got at least one complete sentence; go forth
            %% and process.
            [ process_cluster_data(Socket, D, State) || D <- string:tokens(Pending ++ Data, "\r\n") ],
            {noreply, State#state{pending=""}};
        _ ->
            %% We have at least one incomplete sentence.  Split the input,
            %% process everything but the last item, and add the last one to the pending queue.
            [H|T] = lists:reverse(string:tokens(Pending ++ Data, "\r\n")),
            [ process_cluster_data(Socket, D, State) || D <- lists:reverse(T) ],
            {noreply, State#state{pending=H}}
    end;
handle_info(Info, State) ->
    io:format("GotInfo: ~p~n", [Info]),
    {noreply, State}.

%% @private
terminate(Reason, _State) ->
    io:format("Shutting down: ~p~n", [Reason]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =======================================================================
%% Module API

start_link({peer, _LocalCall, _RemoteCall, _Pass, _Host, _Port}=Peer) ->
    gen_server:start_link(?MODULE, Peer, []).

%% -----------------------------------------------------------------------
%% @doc Connect to a remote DX Spider node.
%% The initialisation sequence (based on information from Dirk, G1TLH; the
%% author of DX Spider) is:
%%
%% <ol>
%% <li>Log in.</li>
%% <li>Receive a PC18. The first field is remote version information; the
%% second is the protocol version.  If the remote version string contains
%% "pc9x" then the node can understand PC9X extensions.</li>
%% <li>Send our config. At a minimum this must be PC92(A) and PC92(K).</li>
%% <li>Send PC20.</li>
%% <li>Receive the other node's config.</li>
%% <li>Receive PC22.</li>
%% </ol>
%%
%% Once the PC22 is received we can consider ourselves connected and
%% ready to exchange spots.
-spec process_connection(any(), string(), #state{}) -> #state{}.
process_connection(_Socket, [], State) ->
    State;
process_connection(Socket, [Line|Rest], State) ->
    process_connection(Socket, Rest, process_line(Socket, Line, State)).

%% -----------------------------------------------------------------------
%% @doc
%-spec process_line() -> any().
process_line(_Socket, [], State) -> State;
process_line(Socket, Line, #state{remoteNode=RemoteNode, localNode=LocalNode}=State) ->
    Bitstring = list_to_bitstring(Line),
    case Bitstring of
        <<"login:", _/binary>> ->
            gen_tcp:send(Socket, LocalNode ++ "\r\n"),
            State;
        <<"PC18", _/binary>> ->
            %% Login succeeded, and the node is talking PC protocol to us.
            send_pc_sentence(Socket, "PC92^" ++ LocalNode ++ "^" ++
                unique_id_server:get_new_id() ++ "^A^^5" ++
                RemoteNode ++"^H99^" ),
            send_pc_sentence(Socket, "PC92^" ++ LocalNode ++ "^" ++
                unique_id_server:get_new_id() ++
                "^K^5" ++ LocalNode ++ ":0.1:0.1^1^1^H99^"),
            send_pc_sentence(Socket, "PC20^"),
            State;
        <<"PC92", _/binary>> ->
            process_pc92(Line),
            State;
        <<"PC22", _/binary>> ->
            State#state{link=connected};
        <<"PC", _/binary>> ->
            io:format("Ignoring ~p before login complete.~n", [Line]),
            State;
        _ ->
            [_H|T] = Line,
            process_line(Socket, T, State)
    end.

process_cluster_data(_Socket, [], _State) ->
    {emptylist};
process_cluster_data(Socket, Data, #state{localNode=LocalNode,remoteNode=RemoteNode}=State) ->
    Bitstring = list_to_bitstring(Data),
    case Bitstring of
        <<"PC61", _/binary>> ->
            process_pc11(Data, State);
        <<"PC11", _/binary>> ->
            process_pc11(Data, State);
        <<"PC24", _/binary>> ->
            %% User presence notifcation
            ignore;
        <<"PC41", _/binary>> ->
            %% Various types of user data (name, location, etc.)
            ignore;
        <<"PC50", _/binary>> ->
            %% Local user count of originating node.
            ignore;
        <<"PC92", _/binary>> ->
            process_pc92(Data);
        <<"PC93", _/binary>> ->
            io:format("Announce from ~p: ~p~n", [RemoteNode, Bitstring]),
            {announce, Data};
        <<"PC51", _/binary>> ->
            send_pc_sentence(Socket, "PC51^" ++ RemoteNode ++ "^" ++ LocalNode ++ "^0^");
        <<"P", _/binary>> ->
            io:format("Received: ~p ~p ~n", [RemoteNode, Bitstring]),
            {unknown_pc, Data};
        _ ->
            [H|T] = Data,
            io:format("Dropping ~p, trying with ~p~n", [H, T]),
            process_cluster_data(Socket, T, State)
    end.

send_pc_sentence(Socket, Data) ->
    gen_tcp:send(Socket, Data ++ "\r\n").

%% -----------------------------------------------------------------------
%% @doc Process an incoming PC92 sentence.
%% PC92 sentences come in 4 varieties and are used for maintaining
%% an accurate picture of the cluster network as a whole.
%%
%% The four types are:
%% <dl>
%% <dt>A</dt>
%% <dd>Add a mixture of clients and nodes to the network.</dd>
%% <dt>C</dt>
%% <dd>A list, sent by a particular node, of all of the clients and nodes
%% connected to it.  This is provided as a fall-back to ensure a
%% consistent view of the network is eventually achieved even if the odd
%% A or D sentence is missed.</dd>
%% <dt>D</dt>
%% <dd>Delete a mixture of clients and nodes from the network.</dd>
%% <dt>K</dt>
%% <dd>Keepalive.</dd>
%% </dl>
-spec process_pc92(string()) -> ok.
process_pc92(Data) ->
    %% The final element is always the hop counter.  We don't care about
    %% it, so drop it to simplify the rest of the processing.
    case utils:butlast(utils:explode(Data, "^")) of
        ["PC92", NodeCall, UniqueId, "A", _ClientNode | Calls ] ->
            cluster:add_nodes(NodeCall, UniqueId, Calls);
        ["PC92", NodeCall, UniqueId, "C", _ClientNode | Calls ] ->
            %% TODO: Bit of a hack; we need to account for deletions.
            cluster:add_nodes(NodeCall, UniqueId, Calls);
        ["PC92", NodeCall, UniqueId, "D", _ClientNode | Calls ] ->
            cluster:delete_nodes(NodeCall, UniqueId, Calls);
        ["PC92", _NodeCall, _UniqueId, "K", _ConnectedCall,
            _ConnectedNodeCount, _ConnectedUsercount] ->
            %% TODO Require keepalives from peer, or reconnect.
            ok
    end,
    ok.

%% -----------------------------------------------------------------------
%% @doc Process an incoming PC11 or PC61 sentence.
%% See {@link node:parse_pc11/1} for differences between the two types.
-spec process_pc11(string(), string()) -> ok.
process_pc11(Data, #state{remoteNode=RemoteNode}=State) ->
    process_pc11(Data,
        fun(X) -> cluster:receive_spot(X, RemoteNode) end,
        State).
%% @hidden
process_pc11(Data, Action, _State) ->
    try parse_pc11(Data) of
        Spot -> Action(Spot)
    catch
        bad_parse -> []
    end,
    ok.

%% @hidden
process_pc11_test_() -> [
    %% Good sentence.
    ?_test(process_pc11("PC11^f^dx^date^time^comment^spotter^src^hops",
                        fun(_) -> ok end)),
    %% We don't want to die and go through the whole process of
    %% reconnecting to the remote node just because we receive a
    %% bad sentence; they should be ignored.
    ?_test(process_pc11("Badness",
                        fun(_) -> ok end))
].

%% -----------------------------------------------------------------------
%% @doc Parse an incoming PC11 or PC61 sentence.
%% Both sentence types represent DX spots; the difference is that
%% PC61 has an extra field representing the user's IP address.
%%
%% The fields are as follows:
%%
%% `"PC11"|Freq|DX|Date|Time|Comment|Spotter|Source Node|Hops'
%% `"PC61"|Freq|DX|Date|Time|Comment|Spotter|Source Node|Spotter IP|Hops'
%%
%% The current implementation ignores the spotter's IP address, so
%% we treat both sentences as equivalent.
%% @throws bad_parse
%% @end
-spec parse_pc11(string()) -> spot().
parse_pc11(Data) ->
    case lists:map(fun string:strip/1, string:tokens(Data, "^")) of
        [_, Qrg, SpottedCall, Date, Time, Comment, SpotterCall|_] ->
            %% Remove any SSID from the SpotterCall
            BareCall = hd(string:tokens(SpotterCall, "-")),
            Iota = case parse_iota(Comment) of
                none -> "";
                Ref -> Ref
            end,
            #spot{spotted_call = SpottedCall,
                  spotter_call = BareCall,
                  comment      = Comment,
                  time         = Time,
                  date         = Date,
                  qrg          = utils:normalise_qrg(Qrg),
                  iota         = Iota};
        _ -> throw(bad_parse)
    end.

%% @hidden
parse_pc11_test_() -> [
    ?_test(parse_pc11("PC11^f^dx^date^time^comment^spotter^src^hops")),
    ?_test(parse_pc11("PC11^f^dx^date^time^comment^spotter^src^ip^hops")),
    ?_assertThrow(bad_parse, parse_pc11("Badness"))
].

%% -----------------------------------------------------------------------
%% @doc Parse an IOTA reference out of random text.
-spec parse_iota(string()) -> string().
parse_iota(String) ->
    UString = string:to_upper(String),
    case re:run(UString, iota_regexp(), [{capture, ['CONT', 'NUM'], list}]) of
        {match, [C,N]} -> io_lib:format("~s-~3..0s", [C, N]);
        nomatch -> none
    end.

%% @hidden
parse_iota_test_() -> [
    ?_assertEqual(lists:flatten(parse_iota("EU-023")), "EU-023"),
    ?_assertEqual(lists:flatten(parse_iota("EU023")), "EU-023"),
    ?_assertEqual(lists:flatten(parse_iota("as023")), "AS-023"),
    ?_assertEqual(lists:flatten(parse_iota("aF23")), "AF-023")
].

iota_regexp() ->
    {ok, Pat} = re:compile("(?<CONT>EU|NA|SA|AS|AF)-?(?<NUM>\\d{2,3})"),
    Pat.
