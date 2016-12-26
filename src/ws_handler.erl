-module(ws_handler).
-behaviour(cowboy_websocket_handler).
-define(TIMEOUT, 10000).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("WS INIT ~n",[]),
    {ok, Req, initial_state, ?TIMEOUT}.

handle_message(Req, [{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], initial_state) ->
    Name = list_to_atom("grid-" ++ integer_to_list(erlang:unique_integer())),
    {ok, _Pid} = conway_sup:start_child(Name, Width, Height),
    conway_gen_serv:start(Name, Width, Height),
    {ok, Req, Name};
handle_message(Req, [{<<"start">>, [{<<"width">>, Width}, {<<"height">>, Height}]}], Name) ->
    conway_gen_serv:start(Name, Width, Height),
    {ok, Req, Name};
handle_message(Req, [{<<"next">>, _}], Name) ->
    %% try conway_gen_serv:next(Name) of
    %% 	Reply -> {reply, {text, Reply}, Req, Name}
    %% catch
    %% 	A:B ->
    %% 	    lager:error("ERROR ~p~p~n",[A,B]),
    %% 	    {ok, Req, Name}
    %% end.
    Reply = conway_gen_serv:next(Name),
    {reply, {text, Reply}, Req, Name}.

websocket_handle({text, Msg}, Req, State) ->
    lager:info("WS HANDLE ~p~n",[Msg]),
    handle_message(Req, jsx:decode(Msg), State);
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, Name) ->
    conway_gen_serv:stop(Name),
    lager:info("WS TERMINATE ~p~n",[Reason]),
    ok.
