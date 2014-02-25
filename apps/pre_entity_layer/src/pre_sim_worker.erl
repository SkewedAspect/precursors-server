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

% API
-export([
	add_entity/3,
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
	entity_states :: list(),
	last_simulate_time :: integer()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

-spec add_entity(EntityID, EntityController, EntityState) -> 'ok' when
	EntityID :: integer(),
	EntityController :: atom(),
	EntityState :: term().

add_entity(EntityID, EntityController, EntityState) ->
	to_entity(cast, add_entity, EntityID, {EntityController, EntityState}).

%% --------------------------------------------------------------------------------------------------------------------

-spec remove_entity(EntityID) -> 'ok' when
	EntityID :: integer().

remove_entity(EntityID) ->
	to_entity(cast, remove_entity, EntityID, undefined).

%% --------------------------------------------------------------------------------------------------------------------

-spec get_entity_state(EntityID) -> {'ok', term()} when
	EntityID :: integer().

get_entity_state(EntityID) ->
	to_entity(call, get_entity_state, EntityID, undefined).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_position(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

update_position(EntityID, Value) ->
	to_entity(cast, update_position, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_linear_momentum(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

update_linear_momentum(EntityID, Value) ->
	to_entity(cast, update_linear_momentum, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_orientation(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: quaternion:quat().

update_orientation(EntityID, Value) ->
	to_entity(cast, update_orientation, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_angular_momentum(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

update_angular_momentum(EntityID, Value) ->
	to_entity(cast, update_angular_momentum, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_absolute(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_force_absolute(EntityID, Value) ->
	to_entity(cast, apply_force_absolute, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_relative(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_force_relative(EntityID, Value) ->
	to_entity(cast, apply_force_relative, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_absolute(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_torque_absolute(EntityID, Value) ->
	to_entity(cast, apply_torque_absolute, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_relative(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_torque_relative(EntityID, Value) ->
	to_entity(cast, apply_torque_relative, EntityID, Value).

%% --------------------------------------------------------------------------------------------------------------------
%% Supervisor-facing API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets up the worker process group and the worker ETS tables for this node.

-spec init_node() -> 'ok'.

init_node() ->
	% Process group for simulation workers.
	pg2:create(pre_sim_worker_group),

	% ETS table that stores the PIDs of worker processes, and the workers' incoming updates table.
	ets:new(pre_sim_worker_table, [public, named_table]),

	% ETS table that stores the mapping between entity IDs and worker processes, with the workers' updates tables.
	ets:new(pre_sim_entity_table, [public, named_table, {keypos, 2}]).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts up with the given arguements.

-spec start_link() -> {'ok', pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts a simulation run on all local workers.

-spec simulate() -> 'ok'.

simulate() ->
	[gen_server:cast(Pid, {simulate, os:timestamp()}) || Pid <- pg2:get_local_members(pre_sim_worker_group)].

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

	InitialState = #state {
		incoming_updates_table = IncomingUpdatesTable
	},

    {ok, InitialState}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

handle_call({get_entity_state, EntityID, _}, _From, State) ->
	Reply = case proplists:get_value(EntityID, State#state.entity_states) of
		undefined ->
			{error, not_found, EntityID};
		{_EntityController, EntityState} ->
			{ok, EntityState}
	end,
    {reply, Reply, State};

handle_call(Request, _From, State) ->
	lager:warn("Unhandled call: ~p", [Request]),
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

handle_cast({add_entity, EntityID, {_EntityController, _EntityState} = Entity}, State) ->
	State1 = State#state{
		entity_states = [{EntityID, Entity} || State#state.entity_states]
	},
    {noreply, State1};

handle_cast({remove_entity, EntityID, _}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, remove}),
    {noreply, State};

handle_cast({update_position, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {position, Value}}),
	{noreply, State};

handle_cast({update_linear_momentum, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {update_linear_momentum, Value}}),
	{noreply, State};

handle_cast({update_orientation, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {update_orientation, Value}}),
	{noreply, State};

handle_cast({update_angular_momentum, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {update_angular_momentum, Value}}),
	{noreply, State};

handle_cast({apply_force_absolute, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {apply_force_absolute, Value}}),
	{noreply, State};

handle_cast({apply_force_relative, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {apply_force_relative, Value}}),
	{noreply, State};

handle_cast({apply_torque_absolute, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {apply_torque_absolute, Value}}),
	{noreply, State};

handle_cast({apply_torque_relative, EntityID, Value}, State) ->
	ets:insert(State#state.incoming_updates_table, {EntityID, {apply_torque_relative, Value}}),
	{noreply, State};

handle_cast({simulate, Start}, State) ->
	State1 = simulate(Start, State),
	{noreply, State1};

handle_cast(Request, State) ->
	lager:warn("Unhandled cast: ~p", [Request]),
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @private

handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% code to handle exits here...
    {noreply, State};

handle_info(Info, State) ->
	lager:warn("Unhandled info: ~p", [Info]),
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

to_entity(CastOrCall, MessageTag, EntityID, MessageArgs) ->
	case ets:lookup(pre_sim_entity_table, EntityID) of
		{error, Error} ->
			{error, Error};
		{ok, Pid} ->
			gen_server:CastOrCall(Pid, {MessageTag, EntityID, MessageArgs})
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

find_entity(EntityID) ->
	case ets:lookup(pre_sim_entity_table, EntityID) of
		[] ->
			{error, not_found};
		[Pid] ->
			{ok, Pid}
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

register_worker(WorkerPid, IncomingUpdatesTable) ->
	case ets:insert_new(pre_sim_worker_table, {WorkerPid, IncomingUpdatesTable} of
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

	% Restart the simulation timer
	erlang:send_after(NextInterval, self(), {self(), simulate}),

	State2 = State1#state {
		last_simulate_time = Start
	},

    {noreply, State2}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

simulate_entities(State) ->
	#state {
		entity_table = EntityTable
	} = State,

	{NewEntities2, OutgoingUpdates2} = lists:foldl(
		fun(Entity, {NewEntities1, OutgoingUpdates1}) ->
			IncomingEntityUpdates = dict:find(Entity#entity.id, Updates),

			% Wrap return_result, passing the new entities and outgoing updates lists.
			ReturnResult = fun(ControllerFuncResult, Context) ->
				return_result(ControllerFuncResult, Context, {NewEntities1, OutgoingUpdates1})
			end,

			% First, apply any incoming updates.
			Entity1 = case IncomingEntityUpdates of
				error -> Entity;
				{ok, []} -> Entity;
				{ok, EntityUpdates} -> entity_controller:apply_updates(lists:flatten(EntityUpdates), Entity)
			end,

			% Then, call simulate.
			entity_controller:call(Entity1, simulate, [Entity1, State])
		end,
		{[], []},
		Entities
	),

	pre_entity_engine_sup:broadcast_updates(OutgoingUpdates2),

	State#state {
		entities = NewEntities2,
		incoming_updates = dict:new()
	}.
