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

	Kids = [
	],
	{ok, { {one_for_one, 5, 10}, Kids} }.
