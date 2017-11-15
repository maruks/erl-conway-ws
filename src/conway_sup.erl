
%% @private
-module(conway_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0, start_child/2]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	{ok, { {one_for_one, 10, 10}, []} }.

start_child(Width, Height) ->
    Id = erlang:unique_integer(),
    supervisor:start_child(?MODULE, conway(conway_gen_server, worker, Id, {Width, Height})).

conway(Module, Type, Id, Args) ->
    {Id, {Module, start_link, [Args]}, transient, 2000, Type, [Module]}.
