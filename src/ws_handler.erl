-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-define(DELAY, 333).
-export([neighbours/2,next_cell_state/2,next_state/1]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

initial_state(Width, Height) ->
    RndKvs =  [ {[X, Y], 1} || X <- lists:seq(0,Width-1), Y <- lists:seq(0,Height-1), rand:uniform(10) < 5],
    [Width, Height, maps:from_list(RndKvs)].

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("WS INIT ~n",[]),
    {ok, Req, inital_state}.

handle_message([{<<"start">>,[{<<"width">>,Width},{<<"height">>,Height}]}]) ->
    erlang:start_timer(?DELAY, self(), ok),
    initial_state(Width, Height).

websocket_handle({text, Msg}, Req, _State) ->
    lager:info("WS HANDLE ~p~n",[Msg]),
    NextState = handle_message(jsx:decode(Msg)),
    {ok, Req, NextState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

neighbours(Grid, [Xp,Yp]) ->
    N = [[X, Y]  || X <- lists:seq(Xp-1,Xp+1) , Y <- lists:seq(Yp-1,Yp+1) , [X,Y] =/= [Xp,Yp]],
    length(lists:filter(fun(P) -> maps:is_key(P, Grid) end, N)).

next_cell_state([X, Y]=P, Grid) ->
    N = neighbours(Grid, [X, Y]),
    cell_state(N,P,Grid ).

cell_state(2 , P , Grid) ->
    maps:is_key(P, Grid);
cell_state(3 , _ ,_ ) ->
    true;
cell_state(N, _P, _Grid) when N>3 ; N<2 ->
    false.

next_state([Width, Height, Grid]) ->
    AliveCells = [[X, Y] || X <- lists:seq(0, Width - 1), Y <- lists:seq(0, Height - 1), next_cell_state([X, Y], Grid)],
    Kvs = lists:map(fun(C) -> {C,1} end, AliveCells),
    [Width, Height, maps:from_list(Kvs)].

websocket_info({timeout, _Ref, _Msg}, Req, [_, _, Grid] = State) ->
    erlang:start_timer(?DELAY, self(), ok),
    {reply, {text,jsx:encode([{alive, maps:keys(Grid)}])}, Req, next_state(State)};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
    lager:info("WS TERMINATE ~p~n",[Reason]),
    ok.
