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
%%% @doc Generate unique IDs for use in PC protocol sentences.
%%% @end
%%% ----------------------------------------------------------------------
-module(unique_id_server).
-behaviour(gen_server).

%% =======================================================================
%% Behaviour Exports
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% =======================================================================
%% Module Exports
-export([start_link/0, get_new_id/0]).

-record(state, {lastSecond=0, lastCounter=0}).
%% =======================================================================
%% Behaviour API

%% @private
init(_Args) ->
    {ok, #state{}}.

%% @private
handle_call({id_request}, _From,
    #state{lastSecond=LastSecond, lastCounter=LastCounter} = State) ->
    {{_, _, _}, {Hours, Mins, Secs}} = calendar:now_to_universal_time(now()),
    SecondsSinceMidnight = (Hours * 3600) + (Mins * 60) + Secs,

    if
        SecondsSinceMidnight == LastSecond ->
            Counter = LastCounter + 1,
            Reply = lists:flatten(io_lib:format("~b.~2.10.0b", [SecondsSinceMidnight, Counter]));
        true ->
            Counter = 0,
            Reply =  lists:flatten(io_lib:format("~b", [SecondsSinceMidnight]))
    end,
    {reply, Reply, State#state{lastSecond=SecondsSinceMidnight,lastCounter=Counter}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =======================================================================
%% Module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_new_id() ->
    gen_server:call(?MODULE, {id_request}).
