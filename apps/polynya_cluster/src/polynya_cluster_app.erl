%%% polynya-cluster
%%% Copyright (C) 2012, Michael Clarke, M0PRL
%%% <clarkema -at- clarkema.org>
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

-module(polynya_cluster_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    polynya_cluster_sup:start_link().

stop(_State) ->
    ok.
