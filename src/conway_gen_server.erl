-module(conway_gen_server).
-behaviour(gen_server).

-import(lists,[map/2,seq/2]).
-import(sets,[from_list/1,is_element/2,to_list/1]).

-export([start_link/1,init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/3,next/1,stop/1,grid/1]).

-ifdef(TEST).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-endif.

-record(state, {width, height, grid}).

start_link({Width, Height}=Args) ->
    lager:info("START ~p~n",[Args]),
    gen_server:start_link(?MODULE, {Width, Height}, []).

init({Width, Height}) ->
    process_flag(trap_exit, true),
    {ok, initial_state(Width, Height)}.

% API

start(Pid, Width, Height) ->
    gen_server:call(Pid, {start, Width, Height}).

grid(Pid) ->
    gen_server:call(Pid, {grid}).

next(Pid) ->
    gen_server:call(Pid, {next}).

stop(Pid) ->
    gen_server:cast(Pid, {stop}).

% internal functions

initial_state(Width, Height) ->
    RndKvs =  [ {X, Y} || X <- seq(0, Width-1), Y <- seq(0, Height-1), rand:uniform(10) < 5],
    #state{width = Width, height = Height, grid = from_list(RndKvs)}.

neighbours(Grid, {Xp,Yp}=P) ->
    length([ {X, Y}  || X <- seq(Xp-1,Xp+1), Y <- seq(Yp-1,Yp+1), {X,Y} =/= P, is_element({X, Y}, Grid)]).

next_cell_state({X, Y}=P, Grid) ->
    N = neighbours(Grid, {X, Y}),
    cell_state(N, P, Grid).

cell_state(2 , P , Grid) ->
    is_element(P, Grid);
cell_state(N , _ ,_ ) ->
    N == 3.

next_grid(Width, Height, Grid) ->
    AliveCells = [{X, Y} || X <- seq(0, Width - 1), Y <- seq(0, Height - 1), next_cell_state({X, Y}, Grid)],
    from_list(AliveCells).

% calls
call({next}, #state{width = Width, height = Height, grid = Grid} = State) ->
    NextGrid = next_grid(Width, Height, Grid),
    {reply, NextGrid, State#state{grid = NextGrid}};
call({grid}, #state{grid = Grid} = State) ->
    {reply, Grid, State};
call({start, Width, Height}, #state{width = Width, height = Height} = State ) ->
    {reply, ok, State};
call({start, Width, Height}, _ ) ->
    {reply, ok, initial_state(Width, Height)}.

% casts
cast({stop}, State) ->
    {stop, normal, State}.

% callbacks

handle_call(Request, _From, State) ->
    call(Request, State).

handle_cast(Request, State) ->
    cast(Request, State).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    lager:info("TERMINATE ~p~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
