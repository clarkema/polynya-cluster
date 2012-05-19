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
%%% @doc
%%% Assorted utility functions that don't belong in any particular
%%% module.
%%% @end
%%% ----------------------------------------------------------------------
-module(utils).
-include_lib("eunit/include/eunit.hrl").
-export([explode/2, butlast/1, cluster_dt_to_iso/2]).

%% -----------------------------------------------------------------------
%% @doc Separate a PC protocol sentence into its fields.
-spec explode(string(), string()) -> [string()].
explode(String, Separator) ->
    lists:reverse(explode(String, Separator, [])).
explode([], _Separator, Result) -> Result;
explode(String, Separator, Result) ->
    Pos = string:str(String, Separator),
    if Pos =:= 0 ->
        [ String | Result ];
    true ->
        NewResult = [string:substr(String, 1, Pos - 1) | Result ],
        explode(string:substr(String, Pos + 1), Separator, NewResult)
    end.

%% @hidden
explode_test_() -> [
    ?_assertEqual(explode("^^^", "^"), [[], [], []]),
    ?_assertEqual(explode("a", "^"), ["a"]),
    ?_assertEqual(explode("a^b^c^", "^"), ["a", "b", "c"])

].

%% -----------------------------------------------------------------------
%% @doc Return all but the last element of a list.
-spec butlast(list()) -> list().
butlast([]) -> [];
butlast(List) ->
    [_|T] = lists:reverse(List),
    lists:reverse(T).

%% @hidden
butlast_test_() -> [
    ?_assertEqual(butlast([]), []),
    ?_assertEqual(butlast([1]), []),
    ?_assertEqual(butlast([1, 2]), [1]),
    ?_assertEqual(butlast([1, 2, 3]), [1, 2])
].

%% -----------------------------------------------------------------------
%% @doc Convert a PC protocol date and time to ISO 8601 basic format.
-spec cluster_dt_to_iso(string(), string()) -> string().
cluster_dt_to_iso(Date, Time) ->
    MonthNumbers = [{"Jan", "01"}, {"Feb", "02"}, {"Mar", "03"}, {"Apr", "04"},
                    {"May", "05"}, {"Jun", "06"}, {"Jul", "07"}, {"Aug", "08"},
                    {"Sep", "09"}, {"Oct", "10"}, {"Nov", "09"}, {"Dec", "12"}],

    [Day, Month, Year] = string:tokens(string:strip(Date), "-"),
    Hour = string:substr(Time, 1, 2),
    Min  = string:substr(Time, 3, 2),

    {_Name, Num} = proplists:lookup(Month, MonthNumbers),
    io_lib:format("~s~s~2..0sT~s~sZ",
        [Year, Num, Day, Hour, Min]).

