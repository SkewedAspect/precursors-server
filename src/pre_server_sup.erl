
-module(pre_server_sup).

-behaviour(supervisor).

-include("log.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> start_link([]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
		ListenerArgs = proplists:get_value(listener, Args, []),
		ListenerKid = ?CHILD(pre_client_sup, supervisor, [ListenerArgs]),
		Kids = [
			ListenerKid
		],
    {ok, { {one_for_one, 5, 10}, Kids} }.

