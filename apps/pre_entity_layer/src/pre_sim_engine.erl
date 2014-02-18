%%% @doc The entity simulation engine.
%%%
%%% This module has exactly one purpose: it runs entity simulations. That is all that it does. There is an exact
%%% one-to-one relationship between entity engine workers and entity engines. The worker also has its own copy of the
%%% entity engine's state. This is stored as a proplist rather than as a dict, for performance reasons.
%%%
%%% The entire reason for this extra process is to move the load of physics simulation to a different process from the
%%% one that has to reply to updates, or communicate with the rest of the system. This process can run at full speed,
%%% only responding to, or generating updates. The rest is handled by the entity event engine.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_sim_engine).

-behaviour(gen_server).

% API
-export([
	start_link/0,
	add_entity/3,
	remove_entity/1,
	get_entity_state/1
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% State record
-record(state, {
	entity_table :: ets:tid(),
	last_simulate :: integer()
}).

% Simulation interval
-define(INTERVAL, 33). % about 1/30th of a second. (33 ms)

% Simulation time at which we start warning about our load.
-define(WARN_INTERVAL, (?INTERVAL - (?INTERVAL * 0.05))). % 5% of Interval

% The number of entities per simulation worker process.
-define(ENTITIES_PER_WORKER, 100).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts up with the given arguements.

-spec start_link() -> {'ok', pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------

-spec add_entity(EntityID, EntityController, EntityState) -> 'ok' when
	EntityID :: integer(),
	EntityController :: atom(),
	EntityState :: term().

add_entity(EntityID, EntityController, EntityState) ->
    gen_server:cast(?MODULE, {add_entity, EntityID, EntityController, EntityState}).

%% --------------------------------------------------------------------------------------------------------------------

-spec remove_entity(EntityID) -> 'ok' when
	EntityID :: integer().

remove_entity(EntityID) ->
    gen_server:cast(?MODULE, {remove_entity, EntityID}).

%% --------------------------------------------------------------------------------------------------------------------

-spec get_entity_state(EntityID) -> {'ok', term()} when
	EntityID :: integer().

get_entity_state(EntityID) ->
    gen_server:call(?MODULE, {get_entity_state, EntityID}).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_position(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

update_position(EntityID, Value) ->
	gen_server:cast(?MODULE, {update_position, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_linear_momentum(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

update_linear_momentum(EntityID, Value) ->
	gen_server:cast(?MODULE, {update_linear_momentum, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_orientation(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: quaternion:quat().

update_orientation(EntityID, Value) ->
	gen_server:cast(?MODULE, {update_orientation, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec update_angular_momentum(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

update_angular_momentum(EntityID, Value) ->
	gen_server:cast(?MODULE, {update_angular_momentum, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_absolute(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_force_absolute(EntityID, Value) ->
	gen_server:cast(?MODULE, {apply_force_absolute, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_relative(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_force_relative(EntityID, Value) ->
	gen_server:cast(?MODULE, {apply_force_relative, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_absolute(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_torque_absolute(EntityID, Value) ->
	gen_server:cast(?MODULE, {apply_torque_absolute, Value}).

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_relative(EntityID, Value) -> 'ok' when
	EntityID :: integer(),
	Value :: vector:vec().

apply_torque_relative(EntityID, Value) ->
	gen_server:cast(?MODULE, {apply_torque_relative, Value}).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

init([]) ->
	lager:info("pre_sim_engine:init()", []),

	% Set our priority to high.
	process_flag(priority, high),

	InitialState = #state {
		entity_table = ets:new(?MODULE, [set, public, named, {keypos, 2}])
	},

	% Start the simulation timer
	erlang:send_after(?INTERVAL, self(), {self(), simulate}),

    {ok, InitialState}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

handle_call({get_entity_state, _EntityID}, _From, State) ->
	%TODO: Query ETS table, and return stuff!
    {reply, [], State};

handle_call(Request, _From, State) ->
	lager:warn("Unhandled call: ~p", [Request]),
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

handle_cast({add_entity, _EntityID, _EntityController, _EntityState}, State) ->
    {noreply, State};

handle_cast({remove_entity, _EntityID}, State) ->
    {noreply, State};

handle_cast({update_position, Value}) ->
	lager:warn("update_position NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast({update_linear_momentum, Value}) ->
	lager:warn("update_linear_momentum NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast({update_orientation, Value}) ->
	lager:warn("update_orientation NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast({update_angular_momentum, Value}) ->
	lager:warn("update_angular_momentum NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast({apply_force_absolute, Value}) ->
	lager:warn("apply_force_absolute NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast({apply_force_relative, Value}) ->
	lager:warn("apply_force_relative NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast({apply_torque_absolute, Value}) ->
	lager:warn("apply_torque_absolute NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast({apply_torque_relative, Value}) ->
	lager:warn("apply_torque_relative NOT IMPLEMENTED!"),
	{noreply, State};

handle_cast(Request, State) ->
	lager:warn("Unhandled cast: ~p", [Request]),
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

handle_info(simulate, State) ->
	%TODO: Spawn worker pids calling simulate/2
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% code to handle exits here...
    {noreply, State};

handle_info(Info, State) ->
	lager:warn("Unhandled info: ~p", [Info]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden

terminate(shutdown, _State) ->
	ok.





%% --------------------------------------------------------------------------------------------------------------------

simulate(Start, State) ->
	LastSim = State#state.last_simulate,
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
		last_simulate = Start
	},

    {noreply, State2}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal API
%% --------------------------------------------------------------------------------------------------------------------

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
