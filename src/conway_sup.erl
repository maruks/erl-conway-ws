
%% @private
-module(conway_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0, start_child/3]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	{ok, { {one_for_one, 10, 10}, []} }.

start_child(Name, Width, Height) ->
    supervisor:start_child(?MODULE, conway(conway_gen_serv, worker, Name, {Name, Width, Height})).

conway(Module, Type, Name, Args) ->
    {Name, {Module, start_link, [Args]}, transient, 2000, Type, [Module]}.
