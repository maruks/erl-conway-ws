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
    Name = list_to_atom("grid-" ++ integer_to_list(erlang:unique_integer())),
    {ok, _Pid} = conway_sup:start_child(Name, Width, Height),
    ok = conway_gen_server:start(Name, Width, Height),
    {ok, Name};
handle_message([{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], Name) ->
    ok = conway_gen_server:start(Name, Width, Height),
    {ok, Name};
handle_message([{<<"next">>, _}], initial_state = S) ->
    erlang:send_after(250, self(), next),
    {ok, S};
handle_message([{<<"next">>, _}], Name) ->
    %% try conway_gen_server:next(Name) of
    %% 	Reply -> {reply, {text, Reply}, Req, Name}
    %% catch
    %% 	A:B ->
    %% 	    lager:error("ERROR ~p~p~n",[A,B]),
    %% 	    {ok, Req, Name}
    %% end.
    Grid = conway_gen_server:next(Name),
    ListGrid = sets:fold(fun({X,Y}, Acc) -> [[X,Y] | Acc] end, [], Grid),
    {reply, {text, jsx:encode([{alive, ListGrid}])}, Name}.

websocket_handle({text, Msg}, State) ->
    handle_message(jsx:decode(Msg), State);
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(next, initial_state = S) ->
    lager:error("Out of order init",[]),
    {ok, S};
websocket_info(next, Name) ->
    handle_message([{<<"next">>, 1}], Name);
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _, Name) ->
    conway_gen_server:stop(Name),
    lager:info("WS TERMINATE ~p~n",[Reason]),
    ok.
