-module(conway_gen_server_test).
-include_lib("eunit/include/eunit.hrl").

-import(sets,[from_list/1]).

%-import(conway_gen_server,[]).


%% server_test_ () ->
%%  {foreach,
%%      fun setup/0,
%%      fun cleanup/1,
%%   [fun updates_and_returns_grid_when_next_is_called/1,
%%    fun resets_grid_if_resized/1]}.

%% updates_and_returns_grid_when_next_is_called(Pid) ->
%%     Grid = conway_sup:next(Pid),
%%     CurrentGrid = conway_sup:grid(Pid),
%%     NewGrid = conway_sup:next(Pid),
%%     [?_assert(sets:is_set(Grid)),
%%      ?_assertNotEqual(Grid, NewGrid),
%%      ?_assertEqual(Grid, CurrentGrid)].

%% resets_grid_if_resized(Pid) ->
%%     Grid = conway_sup:next(Pid),
%%     ok = conway_sup:start(Pid, 30, 30),
%%     CurrentGrid = conway_sup:grid(Pid),
%%     ok = conway_sup:start(Pid, 40, 40),
%%     ResizedGrid = conway_sup:grid(Pid),
%%     [?_assert(sets:is_set(Grid)),
%%      ?_assertNotEqual(Grid, ResizedGrid),
%%      ?_assertEqual(Grid, CurrentGrid)].

%% setup() ->
%%     {ok, Pid} = gen_server:start(conway_gen_server, {30, 30}, []),
%%     Pid.

%% cleanup(Pid) ->
%%     conway_sup:stop(Pid).
