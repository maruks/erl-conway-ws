-module(ws_handler).
-behaviour(cowboy_websocket_handler).

%% -export([init/3]).
%% -export([websocket_init/3]).
%% -export([websocket_handle/3]).
%% -export([websocket_info/3]).
%% -export([websocket_terminate/3]).

%% -export([neighbours/2]).

-compile(export_all).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

initial_state () ->
   RndKvs = lists:map(fun(_) -> {[rand:uniform(120)-1,rand:uniform(60)-1], 1}  end,  lists:seq(1,3000)),
    maps:from_list(RndKvs).

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("WS INIT ~n",[]),
    erlang:start_timer(1000, self(), ok ),
    {ok, Req, initial_state()}.

handle_message(Msg) ->
    lager:info(Msg).

websocket_handle({text, Msg}, Req, State) ->
    lager:info("WS HANDLE ~p~n",[Msg]),
    handle_message(jsx:decode(Msg)),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

neighbours(M, [Xp,Yp]) ->
    N = [[X, Y]  || X <- lists:seq(Xp-1,Xp+1) , Y <- lists:seq(Yp-1,Yp+1) , [X,Y] =/= [Xp,Yp]],
    length(lists:filter(fun(P) -> maps:is_key(P, M) end, N)).

next_cell_state([X, Y], M) ->
    case (neighbours(M, [X, Y])) of
	N when N < 2 -> false;
	2 ->  maps:is_key([X,Y], M);
	3 ->  true;
	N when N > 3 -> false
    end.

next_state(M) ->
    K = [[X, Y] || X <- lists:seq(0, 119), Y <- lists:seq(0, 59)],
    AliveCells = lists:filter(fun(P) -> next_cell_state(P,M) end, K),
    Kvs = lists:map(fun(C) -> {C,1} end, AliveCells),
    maps:from_list(Kvs).

to_list(Map) ->
    maps:keys(Map).

websocket_info({timeout, _Ref, _Msg}, Req, State) ->
    %lager:info("WS TIMER ~p~n",[Msg]),
    erlang:start_timer(333, self(), ok ),
%    Cells = random_cells([],3000),
    {reply, {text,jsx:encode([{alive,to_list(State)}])}, Req, next_state(State)};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
    lager:info("WS TERMINATE ~p~n",[Reason]),
    ok.
