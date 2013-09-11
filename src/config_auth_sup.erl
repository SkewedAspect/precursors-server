-module(config_auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_server/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Args) ->
    {ok, ServerPid} = supervisor:start_child(?MODULE, [Args]),
	ServerPid.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Child = ?CHILD(config_auth, worker, []),
	{ok, {{simple_one_for_one, 5, 10}, [Child]}}.
