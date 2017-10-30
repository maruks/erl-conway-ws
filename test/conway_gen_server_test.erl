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
