%% @doc Top level supervior for pre_data. Simply starts pre_data
%% hard-coded to user pre_mnesia as the backend.
-module(pre_data_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Starts the supervisor named as it's module.
-spec start_link() -> {'ok', pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Exit the {@module} supervisor with reason `normal'.
-spec stop() -> 'normal'.
stop() ->
	exit(whereis(?MODULE), normal).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @hidden
init([]) ->
	pre_mnesia:check_setup(),
	{ok, { {one_for_one, 5, 10}, [?CHILD(pre_data, [pre_mnesia], worker)]} }.

