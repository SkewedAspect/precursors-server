%%% @doc Top level supervisor for the pre_entity_layer app.
%%%
%%% Currently uses the application env variable 'event_workers' to
%%% define the number of gen_event managers the pre_entity_event_sup
%%% will start.
%%%
%%% -------------------------------------------------------------------

-module(pre_entity_engine_sup).
-behaviour(supervisor).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% External API
-export([start_link/0]).

% supervisor
-export([init/1]).

%% --------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------

%% @doc Starts the top level supervisor. Uses 'event_workers' application
%% env to define the number of gen_event managers to start for the
%% {@link pre_entity_event_sup}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------------------------
%% Supervisor
%% --------------------------------------------------------------------

%% @doc private
init(_) ->

	EventWorkers = case application:get_env(pre_entity_layer, event_workers) of
		undefined -> 5;
		Else -> Else
	end,
	EventEngine = {pre_entity_event_sup, {pre_entity_event_sup, start_link, [EventWorkers]},
		permanent, 5, supervisor, [?MODULE]},

	SimulationWorkers = case application:get_env(pre_entity_layer, simulation_workers) of
		undefined -> 5;
		Else1 -> Else1
	end,
	SimulationEngine = {pre_sim_worker_sup, {pre_sim_worker_sup, start_link, [SimulationWorkers]},
		permanent, 5, supervisor, [?MODULE]},

	{ok, {{one_for_one, 5, 10}, [EventEngine, SimulationEngine]}}.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

-ifdef(TEST).

start_test() ->
	Got = start_link(),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	unlink(Pid),
	exit(Pid, kill),
	Mon = erlang:monitor(process, Pid),
	receive
		{'DOWN', Mon, process, Pid, _} ->
			ok
	end.

-endif.

