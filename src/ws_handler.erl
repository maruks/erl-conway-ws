-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-import(lists,[map/2,seq/2]).
-import(maps,[from_list/1,is_key/2,keys/1]).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-define(DELAY, 500).
-export([neighbours/2,next_cell_state/2,next_grid/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

initial_state(Width, Height) ->
    RndKvs =  [ {[X, Y], 1} || X <- seq(0, Width-1), Y <- seq(0, Height-1), rand:uniform(10) < 5],
    [Width, Height, from_list(RndKvs)].

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("WS INIT ~n",[]),
    {ok, Req, inital_state}.

handle_message([{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], [_Ref, Width, Height, _Grid] = State) ->
    State;
handle_message([{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], _) ->
    Ref = erlang:start_timer(?DELAY, self(), ok),
    [Ref | initial_state(Width, Height)].

websocket_handle({text, Msg}, Req, State) ->
    lager:info("WS HANDLE ~p~n",[Msg]),
    NextState = handle_message(jsx:decode(Msg), State),
    {ok, Req, NextState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

neighbours(Grid, [Xp,Yp]) ->
    length([[X, Y]  || X <- seq(Xp-1,Xp+1) , Y <- seq(Yp-1,Yp+1) , [X,Y] =/= [Xp,Yp], is_key([X, Y], Grid)]).

next_cell_state([X, Y]=P, Grid) ->
    N = neighbours(Grid, [X, Y]),
    cell_state(N,P,Grid ).

cell_state(2 , P , Grid) ->
    is_key(P, Grid);
cell_state(N , _ ,_ ) ->
    N == 3.

next_grid(Width, Height, Grid) ->
    AliveCells = [[X, Y] || X <- seq(0, Width - 1), Y <- seq(0, Height - 1), next_cell_state([X, Y], Grid)],
    from_list(map(fun(C) -> {C,1} end, AliveCells)).

websocket_info({timeout, Ref, _Msg}, Req, [Ref, Width, Height, Grid]) ->
    NextGrid = next_grid(Width, Height, Grid),
    Reply = jsx:encode([{alive, keys(NextGrid)}]),
    NewRef = erlang:start_timer(?DELAY, self(), ok),
    {reply, {text, Reply}, Req, [NewRef, Width, Height, NextGrid]};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
    lager:info("WS TERMINATE ~p~n",[Reason]),
    ok.
