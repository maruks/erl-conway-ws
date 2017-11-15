-module(ws_handler).
-define(TIMEOUT, 3600000).
-define(DELAY, 500).

-export([init/2]).

-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, _State) ->
    lager:info("WS INIT ~n",[]),
    {cowboy_websocket, Req, initial_state, #{idle_timeout => ?TIMEOUT}}.

handle_message([{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], initial_state) ->
    {ok, Pid} = conway_sup:start_child(Width, Height),
    ok = conway_gen_server:start(Pid, Width, Height),
    erlang:send_after(?DELAY, self(), next),
    {ok, {started, Pid}};
handle_message([{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], {started, Pid} = S) ->
    ok = conway_gen_server:start(Pid, Width, Height),
    {ok, S};
handle_message(_, S) ->
    {ok, S}.

websocket_handle({text, Msg}, State) ->
    handle_message(jsx:decode(Msg), State);
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(next, {started, Pid}=S) ->
    Grid = conway_gen_server:next(Pid),
    ListGrid = sets:fold(fun({X,Y}, Acc) -> [[X,Y] | Acc] end, [], Grid),
    erlang:send_after(?DELAY, self(), next),
    {reply, {text, jsx:encode([{alive, ListGrid}])}, S};
websocket_info(_, State) ->
    {ok, State}.

terminate(Reason, _, {started, Pid}) ->
    conway_gen_server:stop(Pid),
    lager:info("WS TERMINATE ~p~n",[Reason]),
    ok;
terminate(_, _, _) ->
    ok.
