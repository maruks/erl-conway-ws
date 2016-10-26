-module(tests).
-include_lib("eunit/include/eunit.hrl").

neighbours_test() ->
    Grid = #{[1,1] => 1},
    N = ws_handler:neighbours(Grid,[0,0]),
    ?assert(N == 1).

neighbours2_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1},
    N = ws_handler:neighbours(Grid,[2,1]),
    ?assert(N == 2).

survival_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1},
    CellState = ws_handler:next_cell_state([2,1],Grid),
    ?assert(CellState == false).

survival2_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1,[2,1] => 1, [2,2] => 1},
    CellState = ws_handler:next_cell_state([2,1],Grid),
    ?assert(CellState == true).

next_grid_test() ->
    Grid = #{[1,1] => 1,[1,2] => 1, [2,2] => 1},
    NextGrid = ws_handler:next_grid(4, 4, Grid),
    ExpectedGrid = #{[1,1] => 1, [1,2] => 1, [2,1] => 1, [2,2] => 1},
    ?assert(NextGrid == ExpectedGrid).
