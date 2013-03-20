%%% @doc An entity representing a physical object.

-module(entity_physical).

-include("log.hrl").
-include("pre_entity.hrl").
-include("pre_physics.hrl").

% pre_entity
-export([init/2, get_full_state/1, client_request/6, client_event/5, timer_fired/2]).

-define(STEP_SIZE, 50).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

init(EntityID, Behavior) ->
	pre_entity_engine:start_entity_timer(EntityID, ?STEP_SIZE, do_physics),
	#entity{
		id = EntityID,
		behavior = Behavior,
		state = [
			{physical, #physical{ last_update = os:timestamp() }}
		]
	}.

%% -------------------------------------------------------------------

get_full_state(EntityState) ->
	#physical{
		% Updated values (assume these change every frame)
		position = Position,
		linear_momentum = LinearMomentum,
		orientation = Orientation,
		angular_momentum = AngularMomentum,

		% Input-only values
		force_absolute = AbsoluteForce,
		force_relative = RelativeForce,
		torque_absolute = AbsoluteTorque,
		torque_relative = RelativeTorque,

		% Purely calculated values (DON'T try to change these externally)
		linear_velocity = LinearVelocity,
		angular_velocity = AngularVelocity,

		% Intrinsic values (should NOT change during the life of an object)
		mass = Mass,
		inverse_mass = InverseMass,
		inertia_tensor = InertiaTensor,
		inverse_inertia_tensor = InverseInertiaTensor
	} = proplists:get_value(physical, EntityState#entity.state),

	FullState = [
		{behavior, <<"Physical">>},

		{position, vector:vec_to_list(Position)},
		{linear_momentum, vector:vec_to_list(LinearMomentum)},
		{orientation, quaternion:quat_to_list(Orientation)},
		{angular_momentum, vector:vec_to_list(AngularMomentum)},

		{force_absolute, vector:vec_to_list(AbsoluteForce)},
		{force_relative, vector:vec_to_list(RelativeForce)},
		{torque_absolute, vector:vec_to_list(AbsoluteTorque)},
		{torque_relative, vector:vec_to_list(RelativeTorque)},

		{linear_velocity, vector:vec_to_list(LinearVelocity)},
		{angular_velocity, vector:vec_to_list(AngularVelocity)},

		{mass, Mass},
		{inverse_mass, InverseMass},
		{inertia_tensor, InertiaTensor},
		{inverse_inertia_tensor, InverseInertiaTensor}
	],

	{FullState, EntityState}.

%% -------------------------------------------------------------------

client_request(EntityState, _ClientInfo, Channel, RequestType, _RequestID, Request) ->
	?debug("~p received invalid request ~p on channel ~p! (full request: ~p)",
		[EntityState#entity.id, RequestType, Channel, Request]),
	Response = {reply, [
		{confirm, false},
		{reason, <<"VALID CRAPBACK: Invalid request!">>}
	]},
	{Response, EntityState}.

%% -------------------------------------------------------------------

client_event(EntityState, _ClientInfo, Channel, EventType, Event) ->
	?debug("~p received invalid event ~p on channel ~p! (full event: ~p)",
		[EntityState#entity.id, EventType, Channel, Event]),
	{noreply, EntityState}.

%% -------------------------------------------------------------------

timer_fired(EntityState, do_physics) ->
	LastPhysical = proplists:get_value(physical, EntityState#entity.state),
	#physical{
		last_update = LastUpdate
	} = LastPhysical,
	ThisUpdate = os:timestamp(),
	Physical = pre_physics_rk4:simulate(timer:now_diff(ThisUpdate, LastUpdate) / 1000000, LastPhysical),
	EntityState1 = EntityState#entity{
		state = lists:keystore(physical, 1, EntityState#entity.state,
			{physical, Physical#physical{ last_update = ThisUpdate }}
		)
	},
	{noreply, EntityState1}.
