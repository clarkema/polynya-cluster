
-module(polynya_cluster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Links} = application:get_env(polynya_cluster, links),
    Children = [
        ?CHILD(cluster, worker),
        ?CHILD(unique_id_server, worker)
    ] ++ [ childspec(P) || P <- Links ],
    {ok, {{one_for_one, 5, 10}, Children}}.

childspec({peer, _, PeerCall, _, _, _} = Peer) ->
    {PeerCall, {node, start_link, [Peer]}, temporary, 5000, worker, [node]}.
