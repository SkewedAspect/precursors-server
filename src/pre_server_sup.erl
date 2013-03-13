%% @doc Top level supervisor for precursors server.
-module(pre_server_sup).

-behaviour(supervisor).

-include("log.hrl").

%% API
-export([start_link/0,start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Starts the server with all default options.
%% @see start_link/1
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() -> start_link([]).

%% @doc Starts the server with the given options.
-spec(start_link/1 :: (Args :: [any()]) -> {'ok', pid()}).
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @hidden
init(Args) ->
	ListenerArgs = proplists:get_value(listener, Args, []),
	ListenerKid = ?CHILD(pre_client_sup, supervisor, [ListenerArgs]),

	HooksKid = ?CHILD(pre_hooks, supervisor, []),

	DataKid = ?CHILD(pre_data, worker, []),

	EntityManagerArgs = proplists:get_value(entity_engine_sup, Args, []),
	EntityManagerKid = ?CHILD(pre_entity_engine_sup, supervisor, [EntityManagerArgs]),

	EntityChannelArgs = proplists:get_value(channel_entity_sup, Args, []),
	EntityChannelKid = ?CHILD(pre_channel_entity_sup, supervisor, [EntityChannelArgs]),

	AuthManagerArgs = proplists:get_value(auth_backends, Args, []),
	AuthManagerKid = ?CHILD(pre_gen_auth, worker, [AuthManagerArgs]),

	% Load any plugins
	PluginsToLoad = proplists:get_value(plugins, Args, []),
	timer:apply_after(10, pre_util, start_apps, [PluginsToLoad]),

	Kids = [
		AuthManagerKid,
		ListenerKid,
		HooksKid,
		DataKid,
		EntityManagerKid,
		EntityChannelKid
	],
	{ok, { {one_for_one, 5, 10}, Kids} }.
