-module(conway_gen_server_test).
-include_lib("eunit/include/eunit.hrl").

-import(conway_gen_serv,[neighbours/2,next_cell_state/2,next_grid/3]).

neighbours_test() ->
    Grid = #{[1,1] => 1},
    N = neighbours(Grid,[0,0]),
    ?assert(N == 1).

neighbours2_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1},
    N = neighbours(Grid,[2,1]),
    ?assert(N == 2).

survival_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1},
    CellState = next_cell_state([2,1],Grid),
    ?assert(CellState == false).

survival2_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1,[2,1] => 1, [2,2] => 1},
    CellState = next_cell_state([2,1],Grid),
    ?assert(CellState == true).

next_grid_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1, [2,2] => 1},
    NextGrid = next_grid(4, 4, Grid),
    ExpectedGrid = #{[1,1] => 1, [1,2] => 1, [2,1] => 1, [2,2] => 1},
    ?assert(NextGrid == ExpectedGrid).

server_test_ () ->
 {foreach,
     fun setup/0,
     fun cleanup/1,
  [fun updates_and_returns_grid_when_next_is_called/1,
   fun resets_grid_if_resized/1]}.

updates_and_returns_grid_when_next_is_called(Pid) ->
    Grid = conway_gen_serv:next(Pid),
    CurrentGrid = conway_gen_serv:grid(Pid),
    NewGrid = conway_gen_serv:next(Pid),
    [?_assert(is_list(Grid)),
     ?_assertNotEqual(Grid, NewGrid),
     ?_assertEqual(Grid, CurrentGrid)].

resets_grid_if_resized(Pid) ->
    Grid = conway_gen_serv:next(Pid),
    ok = conway_gen_serv:start(Pid, 30, 30),
    CurrentGrid = conway_gen_serv:grid(Pid),
    ok = conway_gen_serv:start(Pid, 40, 40),
    ResizedGrid = conway_gen_serv:grid(Pid),
    [?_assert(is_list(Grid)),
     ?_assertNotEqual(Grid, ResizedGrid),
     ?_assertEqual(Grid, CurrentGrid)].

setup() ->
    {ok, Pid} = gen_server:start(conway_gen_serv, {30, 30}, []),
    Pid.

cleanup(Pid) ->
    conway_gen_serv:stop(Pid).
