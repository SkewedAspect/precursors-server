-module(mongo_auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_server/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Args) ->
	ServerDef = ?CHILD(mongo_auth, worker, Args),
    {ok, ServerPid} = supervisor:start_child(?MODULE, ServerDef),
	ServerPid.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{one_for_one, 5, 10}, []}}.