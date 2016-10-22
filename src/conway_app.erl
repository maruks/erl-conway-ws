%% @private
-module(conway_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
        {ok, PortNum} = application:get_env(port),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, conway, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, conway, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, PortNum}],
		[{env, [{dispatch, Dispatch}]}]),
        lager:info("Started on port ~p~n",[PortNum]),
	conway_sup:start_link().

stop(_State) ->
	ok.
