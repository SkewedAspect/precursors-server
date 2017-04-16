%%% @doc The entity simulation worker.
%%%
%%% This module has exactly one purpose: it runs entity simulations. That is all that it does.
%%%
%%% The worker owns the authoritative copy of each entity's physical state. This is stored as a proplist rather than as
%%% a dict, for performance reasons. Incoming update requests are stored in a dict for easy lookup, and applied at the
%%% next `simulate' call.
%%%
%%% The entire reason for this extra process is to move the load of physics simulation to a different process from the
%%% one that has to reply to updates, or communicate with the rest of the system. This process can run at full speed,
%%% only responding to, or generating updates. The rest is handled by the entity event engine.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_sim_worker).

-behaviour(gen_server).

-include("simulation.hrl").

% API
-export([
	add_entity/4,
	remove_entity/1,
	get_entity_state/1,
	update_position/2,
	update_linear_momentum/2,
	update_orientation/2,
	update_angular_momentum/2,
	apply_force_absolute/2,
	apply_force_relative/2,
	apply_torque_absolute/2,
	apply_torque_relative/2
]).

% Supervisor-facing API
-export([
	init_node/0,
	start_link/0,
	simulate/0
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% State record
-record(state, {
	incoming_updates_table :: ets:tid(),
	entity_states = [] :: list(),
	last_simulate_time :: integer()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

-spec add_entity(WorkerPid, EntityID, EntityController, EntityState) -> Reply when
	WorkerPid :: pid(),
	EntityID :: integer(),
	EntityController :: atom(),
	EntityState :: term(),
	Reply :: 'ok' | {'error', term()}.

add_entity(WorkerPid, EntityID, EntityController, EntityState) ->
	case gen_server:call(WorkerPid, {add_entity, EntityID, {EntityController, EntityState}}) of
		ok ->
			case ets:update_element(pre_sim_entity_table, EntityID, {2, WorkerPid}) of
				true -> ok;
				false ->
					case ets:lookup(pre_sim_worker_table, WorkerPid) of
						[] ->
							{error, worker_not_found};
						[{WorkerPid, WorkerUpdatesTable}] ->
							ets:insert(pre_sim_entity_table, {EntityID, WorkerPid, WorkerUpdatesTable}),
							ok
					end
			end;
		Error -> Error
	end.

%% --------------------------------------------------------------------------------------------------------------------

-spec remove_entity(EntityID) -> Reply when
	EntityID :: integer(),
	Reply :: 'ok' | {'error', term()}.

remove_entity(EntityID) ->
	add_entity_update(EntityID, remove).

%% --------------------------------------------------------------------------------------------------------------------

-spec get_entity_state(EntityID) -> Reply when
	EntityID :: integer(),
	Reply :: {'ok', EntityState} | {'error', term()},
	EntityState :: term().

get_entity_state(EntityID) ->
	call_to_entity(get_entity_state, EntityID, undefined).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_position(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: vector:vec(),
	Reply :: 'ok' | {'error', term()}.

update_position(EntityID, Value) ->
	add_entity_update(EntityID, {update_position, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_linear_momentum(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: vector:vec(),
	Reply :: 'ok' | {'error', term()}.

update_linear_momentum(EntityID, Value) ->
	add_entity_update(EntityID, {update_linear_momentum, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_orientation(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: quaternion:quat(),
	Reply :: 'ok' | {'error', term()}.

update_orientation(EntityID, Value) ->
	add_entity_update(EntityID, {update_orientation, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_angular_momentum(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: vector:vec(),
	Reply :: 'ok' | {'error', term()}.

update_angular_momentum(EntityID, Value) ->
	add_entity_update(EntityID, {update_angular_momentum, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_absolute(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: vector:vec(),
	Reply :: 'ok' | {'error', term()}.

apply_force_absolute(EntityID, Value) ->
	add_entity_update(EntityID, {apply_force_absolute, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_relative(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: vector:vec(),
	Reply :: 'ok' | {'error', term()}.

apply_force_relative(EntityID, Value) ->
	add_entity_update(EntityID, {apply_force_relative, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_absolute(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: vector:vec(),
	Reply :: 'ok' | {'error', term()}.

apply_torque_absolute(EntityID, Value) ->
	add_entity_update(EntityID, {apply_torque_absolute, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_relative(EntityID, Value) -> Reply when
	EntityID :: integer(),
	Value :: vector:vec(),
	Reply :: 'ok' | {'error', term()}.

apply_torque_relative(EntityID, Value) ->
	add_entity_update(EntityID, {apply_torque_relative, Value}).

%% --------------------------------------------------------------------------------------------------------------------
%% supervisor-facing API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets up the worker process group and the worker ETS tables for this node.

-spec init_node() -> 'ok'.

init_node() ->
	% Process group for simulation workers.
	pg2:create(pre_sim_worker_group),

	% ETS table that stores the PIDs of worker processes, and the workers' incoming updates table.
	% {WorkerPID, WorkerUpdatesTable}
	ets:new(pre_sim_worker_table, [public, named_table]),

	% ETS table that stores the mapping between entity IDs and worker processes, with the workers' updates tables.
	% {EntityID, WorkerPID, WorkerUpdatesTable}
	ets:new(pre_sim_entity_table, [public, named_table]).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts up with the given arguements.

-spec start_link() -> {'ok', pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts a simulation run on all local workers.

-spec simulate() -> 'ok'.

simulate() ->
	Now = os:timestamp(),
	[gen_server:cast(Pid, {simulate, Now}) || Pid <- pg2:get_local_members(pre_sim_worker_group)].

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

%% @private

init([]) ->
	lager:info("pre_sim_worker:init()", []),

	% Set our priority to high.
	process_flag(priority, high),

	% Join the simulation worker process group.
	pg2:join(pre_sim_worker_group, self()),

	% ETS table that stores incoming updates for entities owned by this worker.
	IncomingUpdatesTable = ets:new(?MODULE, [bag, public, {write_concurrency, true}]),

	register_worker(self(), IncomingUpdatesTable),

	InitialState = #state{
		incoming_updates_table = IncomingUpdatesTable
	},

    {ok, InitialState}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

handle_call({add_entity, EntityID, {_EntityController, _EntityState} = Entity}, _From, State) ->
	State1 = State#state{
		entity_states = [{EntityID, Entity} | State#state.entity_states]
	},
    {reply, ok, State1};

handle_call({get_entity_state, EntityID, _}, _From, State) ->
	Reply = case proplists:get_value(EntityID, State#state.entity_states) of
		undefined ->
			{error, not_found};
		{_EntityController, EntityState} ->
			{ok, EntityState}
	end,
    {reply, Reply, State};

handle_call(Request, _From, State) ->
	lager:warning("Unhandled call: ~p", [Request]),
    {reply, {error, invalid}, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

handle_cast({simulate, Start}, State) ->
	State1 = simulate(Start, State),
	{noreply, State1};

handle_cast(Request, State) ->
	lager:warning("Unhandled cast: ~p", [Request]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

handle_info({'EXIT', _Pid, _Reason}, State) ->
	%% code to handle exits here...
	{noreply, State};

handle_info(Info, State) ->
	lager:warning("Unhandled info: ~p", [Info]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

terminate(shutdown, _State) ->
	ok.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal API
%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

call_to_entity(MessageTag, EntityID, MessageArgs) ->
	case ets:lookup(pre_sim_entity_table, EntityID) of
		[] ->
			{error, not_found};
		[{EntityID, WorkerPid, _WorkerUpdatesTable}] ->
			gen_server:call(WorkerPid, {MessageTag, EntityID, MessageArgs})
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

add_entity_update(EntityID, Update) ->
	case ets:lookup(pre_sim_entity_table, EntityID) of
		[] ->
			{error, not_found};
		[{EntityID, _WorkerPid, WorkerUpdatesTable}] ->
			ets:insert(WorkerUpdatesTable, {EntityID, Update}),
			ok
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

register_worker(WorkerPid, IncomingUpdatesTable) ->
	case ets:insert_new(pre_sim_worker_table, {WorkerPid, IncomingUpdatesTable}) of
		true -> true;
		false ->
			lager:error("Error inserting worker info for %p! (IncomingUpdatesTable = %p)",
				[WorkerPid, IncomingUpdatesTable])
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

simulate(Start, State) ->
	LastSim = State#state.last_simulate_time,
	SimGap = if
		LastSim == undefined ->
			0;
		true ->
			(timer:now_diff(Start, LastSim) / 1000)
	end,

	if
		SimGap >= (?INTERVAL + (?INTERVAL * 0.25)) ->
			lager:error("Extremely long time between simulate calls: Entity Engine: ~p, time: ~p", [self(), SimGap]);

		SimGap >= (?INTERVAL + (?INTERVAL * 0.1)) ->
			lager:warning("Overly long time between simulate calls: Entity Engine: ~p, time: ~p", [self(), SimGap]);

		true -> ok
	end,

	% Simulate all our entities
	State1 = simulate_entities(State),

	% Record the amount of time taken to perform simulate
	SimTime = (timer:now_diff(erlang:now(), Start) / 1000),

	% Report if there's issues.
	NextInterval = if
		SimTime >= ?INTERVAL ->
			lager:error("Overloaded Entity Engine ~p (~p ms).", [self(), SimTime]),
			0;
		SimTime >= ?WARN_INTERVAL ->
			lager:warning("High Load on Entity Engine ~p (~p ms).", [self(), SimTime]),
			round(?INTERVAL - SimTime);
		true ->
			round(?INTERVAL - SimTime)
	end,

	State1#state {
		last_simulate_time = Start
	}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

get_pending_updates(EntityID, IncomingUpdatesTable, Entity) when not is_list(IncomingUpdatesTable) ->
	EntityUpdates = ets:lookup(IncomingUpdatesTable, EntityID),
	ets:delete(IncomingUpdatesTable, EntityID),
	EntityUpdates.

%% @hidden

simulate_entities(State) ->
	#state {
		entity_states = PreviousEntityStates,
		incoming_updates_table = IncomingUpdatesTable
	} = State,

	{EntityStates2, OutgoingUpdates3} = lists:foldl(
		fun({EntityID, {EntityController, Entity1}}, {EntityStates1, OutgoingUpdates1}) ->
			% First, apply any incoming updates.
			Updates = get_pending_updates(EntityID, IncomingUpdatesTable, Entity1),

			% Then, call simulate.
			{OutgoingUpdates2, Entity2} = EntityController:simulate(Updates, Entity1),

			{[{EntityID, {EntityController, Entity2}} | EntityStates1], lists:append(OutgoingUpdates1, OutgoingUpdates2)}
		end,
		{[], []},
		PreviousEntityStates
	),

	lager:warning("TODO: Broadcast updates to nearby entities!"),
	%pre_entity_engine_sup:broadcast_updates(OutgoingUpdates3),

	State#state {
		entity_states = EntityStates2
	}.
