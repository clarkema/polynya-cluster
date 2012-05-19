%%% polynya-cluster
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
%%% @doc A 'virtual' cluster, muxing data from multiple nodes.
%%% <p>
%%% Although polynya-cluster can connect to multiple cluster nodes
%%% simultaneously, it's convenient to be able to deal with a single
%%% 'virtual' cluster.  This module provides exactly that; individual
%%% nodes connect to their clusters, and forward data received to the
%%% server defined here.  This module:
%%% </p>
%%% <ul>
%%% <li>takes care of any processing required.</li>
%%% <li>forwards a single aggregated stream of spots to the database.</li>
%%% </ul>
%%% <p>
%%% Individual nodes should never talk to any process other than
%%% this one.
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------
-module(cluster).
-behaviour(gen_server).
-include("spot.hrl").
-include_lib("eunit/include/eunit.hrl").

%% =======================================================================
%% Behaviour Exports
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% =======================================================================
%% Module Exports
-export([start_link/0, receive_spot/1, add_nodes/3, delete_nodes/3,
         map/0]).

%% =======================================================================
%% Behaviour API
-record(state, {nodes, spotdb}).

%% @private
init(_Args) ->
    %Nodes = orddict:new(),
    %ProxyPid = clusterproxy:connect_new(),
    %orddict:store(ProxyPid, ProxyPid, Proxies),
    %loop(#state{proxies=Proxies}).
    ets:new(network, [bag, named_table]),
    Server = couchbeam:server_connection("127.0.0.1", "5984", "", []),
    {ok, _Version} = couchbeam:server_info(Server),
    {ok, SpotDB}   = couchbeam:open_or_create_db(Server, "spots", []),
    {ok, #state{spotdb=SpotDB}}.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

sha_hash(Term) ->
    <<Hash:160/integer>> = crypto:sha(Term),
    lists:flatten(io_lib:format("~40.16.0b", [Hash])).

spot_to_json(#spot{spotted_call = SpottedCall,
                   spotter_call = SpotterCall,
                   comment      = Comment,
                   time         = Time,
                   date         = Date,
                   qrg          = Qrg}) ->
    IsoDateTime = utils:cluster_dt_to_iso(Date, Time),
    Hash = sha_hash( [IsoDateTime, SpottedCall,
                      SpotterCall, Qrg, Comment ]),
    {[{<<"_id">>,          list_to_bitstring(Hash)},
      {<<"spotted_call">>, list_to_bitstring(SpottedCall)},
      {<<"spotter_call">>, list_to_bitstring(SpotterCall)},
      {<<"comment">>,      list_to_bitstring(Comment)    },
      {<<"datetime">>,     list_to_bitstring(IsoDateTime)},
      {<<"qrg">>,          list_to_bitstring(Qrg)}]}.
%% @private
handle_cast({receive_spot, Text}, #state{spotdb=SpotDB}=State) ->
    io:format("Spot: ~p~n", [Text]),
    couchbeam:save_doc(SpotDB, spot_to_json(Text)),
    {noreply, State};
handle_cast({add_nodes, NodeCall, _UniqueId, Calls}, State) ->
    [ets:insert(network, {NodeCall, strip_call(C)})
        || C <- Calls, nodep(C)],
    {noreply, State};
handle_cast({delete_nodes, NodeCall, _UniqueId, Calls}, State) ->
    [ets:delete(network, {NodeCall, C}) || C <- Calls],
    {noreply, State};
handle_cast(print_network, State) ->
    write_graph(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ets:delete(network),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =======================================================================
%% Module API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% -----------------------------------------------------------------------
%% @doc Process a newly-received spot.
%% <p>Intended to be called by a node on receipt of a spot, this
%% will process the spot and add it to the database.</p>
receive_spot(Spot) ->
    gen_server:cast(cluster, {receive_spot, Spot}).

%% -----------------------------------------------------------------------
%% @doc 
add_nodes(NodeCall, UniqueId, Calls) ->
    gen_server:cast(cluster, {add_nodes, NodeCall, UniqueId, Calls}),
    ok.

%% -----------------------------------------------------------------------
%% @doc 
delete_nodes(NodeCall, UniqueId, Calls) ->
    gen_server:cast(cluster, {delete_nodes, NodeCall, UniqueId, Calls}),
    ok.

%% -----------------------------------------------------------------------
%% @doc 
map() ->
    gen_server:cast(cluster, print_network).

%% -----------------------------------------------------------------------
%% @doc
%-spec map_elements() -> any().
map_elements(Fun) ->
    map_elements(Fun, ets:first(network)).
map_elements(_Fun, '$end_of_table') -> [];
map_elements(Fun, Element) ->
    Fun(Element),
    map_elements(Fun, ets:next(network, Element)).

         
%% -----------------------------------------------------------------------
%% @doc 
%-spec write_graph() -> any().
write_graph() ->
    {ok, FH} = file:open("graph.dot", [write]),
    io:format(FH, "graph network {~n", []),
    map_elements(fun(X) ->
        [io:format(FH, "~p -- ~p~n", [H, C]) || {H, C} <- ets:lookup(network, X)]
    end),
    io:format(FH, "}~n", []),
    file:close(FH).


%% -----------------------------------------------------------------------
%% @doc 
%-spec strip_call() -> any().
strip_call(String) ->
    string:substr(hd(string:tokens(String, ":")), 2).
    
%% @hidden
strip_call_test_() -> [
    ?_assertEqual(strip_call("1SP5IT:87.205.147.218"), "SP5IT")
].


%% -----------------------------------------------------------------------
%% @doc 
-spec nodep(string()) -> boolean().
nodep(String) ->
    Bitstring = list_to_bitstring(String),
    case Bitstring of
        <<"3", _/binary>> -> true;
        <<"5", _/binary>> -> true;
        _ -> false
    end.
    
