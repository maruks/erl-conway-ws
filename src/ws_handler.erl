-module(ws_handler).
-define(TIMEOUT, 3600000).

-export([init/2]).

-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, _State) ->
    lager:info("WS INIT ~n",[]),
    {cowboy_websocket, Req, initial_state, #{idle_timeout => ?TIMEOUT}}.

handle_message([{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], initial_state) ->
    {ok, Pid} = conway_sup:start_child(Width, Height),
    ok = conway_sup:start(Pid, Width, Height),
    {ok, Pid};
handle_message([{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], Pid) ->
    ok = conway_sup:start(Pid, Width, Height),
    {ok, Pid};
handle_message([{<<"next">>, _}], initial_state = S) ->
    erlang:send_after(500, self(), next),
    {ok, S};
handle_message([{<<"next">>, _}], Pid) ->
    Grid = conway_sup:next(Pid),
    ListGrid = sets:fold(fun({X,Y}, Acc) -> [[X,Y] | Acc] end, [], Grid),
    {reply, {text, jsx:encode([{alive, ListGrid}])}, Pid}.

websocket_handle({text, Msg}, State) ->
    handle_message(jsx:decode(Msg), State);
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(next, initial_state = S) ->
    lager:error("Out of order init",[]),
    {stop, S};
websocket_info(next, Pid) ->
    handle_message([{<<"next">>, 1}], Pid);
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _, Pid) ->
    conway_sup:stop(Pid),
    lager:info("WS TERMINATE ~p~n",[Reason]),
    ok.
