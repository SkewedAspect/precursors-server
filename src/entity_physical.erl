%%% @doc An entity representing a physical object.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_physical).

-behaviour(entity_controller).

% API
-export([get_orientation/1, get_linear_velocity/1, get_angular_velocity/1, get_force_relative/1, get_torque_relative/1]).

% entity_controller
-export([init/3, simulate/2, get_full_state/2, client_request/6, client_event/5, entity_event/4, apply_update/4]).

-record(state, {
	physical :: any(),

	id :: term(),
	controller :: module(),
	base :: any()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

get_orientation(State) ->
	pre_physics_rk4:get_prop(orientation, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_linear_velocity(State) ->
	pre_physics_rk4:get_prop(linear_velocity, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_angular_velocity(State) ->
	pre_physics_rk4:get_prop(angular_velocity, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_force_relative(State) ->
	pre_physics_rk4:get_prop(force_relative, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_torque_relative(State) ->
	pre_physics_rk4:get_prop(torque_relative, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------
%% entity_controller
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, Controller, InitData) ->
	#state{
		physical = pre_physics_rk4:update_from_proplist(pre_physics_rk4:default_physical(), InitData),

		id = EntityID,
		controller = Controller,
		base = entity_base:init(EntityID, Controller, InitData)
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(_Controller, State) ->
	LastPhysical = State#state.physical,

	% Do physics simulation
	Physical = pre_physics_rk4:simulate(LastPhysical),

	% Calculate a state update message
	PhysicalPL = pre_physics_rk4:to_proplist(Physical),
	LastPhysicalPL = pre_physics_rk4:to_proplist(LastPhysical),
	UpdateMsg = entity_base:diff_state(LastPhysicalPL, PhysicalPL),

	% Save State
	NewState = State#state{
		physical = Physical
	},

	{UpdateMsg, NewState}.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(Controller, State) ->
	{_, BaseFullState} = entity_base:get_full_state(Controller, State#state.base),

	{<<"Physical">>, [
		{physical, pre_physics_rk4:to_proplist(State#state.physical)}
		| BaseFullState
	]}.

%% --------------------------------------------------------------------------------------------------------------------

client_request(Channel, RequestType, RequestID, Request, Controller, State) ->
	{Update, NewBase} = entity_base:client_request(Channel, RequestType, RequestID, Request, Controller, State),
	{Update, State#state { base = NewBase }}.

%% --------------------------------------------------------------------------------------------------------------------

client_event(Channel, EventType, Event, Controller, State) ->
	{Update, NewBase} = entity_base:client_event(Channel, EventType, Event, Controller, State),
	{Update, State#state { base = NewBase }}.

%% --------------------------------------------------------------------------------------------------------------------

entity_event(Event, From, Controller, State) ->
	{Update, NewBase} = entity_base:client_event(Event, From, Controller, State),
	{Update, State#state { base = NewBase }}.

%% --------------------------------------------------------------------------------------------------------------------

apply_update(physical, PhysicalUpdates, _Controller, State) ->
	NewPhysical = pre_physics_rk4:update_from_proplist(State#state.physical, PhysicalUpdates),

	State1 = State#state {
		physical = NewPhysical
	},
	{[], State1};

apply_update(Key, SubKeys, Controller, State) ->
	{Update, NewBase} = entity_base:apply_update(Key, SubKeys, Controller, State),
	{Update, State#state { base = NewBase }}.
