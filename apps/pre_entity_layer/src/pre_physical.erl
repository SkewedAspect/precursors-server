%%% @doc The physical state of an object.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_physical).

-compile([{parse_transform, rec2json}]).

% API
-export([
	new/0,
	apply_force_absolute/2,
	apply_force_relative/2,
	apply_torque_absolute/2,
	apply_torque_relative/2
]).

% Physical object record
-record(?MODULE, {
	% The ID of this pre_physical object (generally set to the matching entity's ID)
	id :: any(),

	% Updated values (assume these change every frame)
	position = vector:zero() :: vector:vec(),
	linear_momentum = vector:zero() :: vector:vec(),
	orientation = quaternion:identity() :: quaternion:quat(),
	angular_momentum = vector:zero() :: vector:vec(),

	% Input-only values
	force_absolute = vector:zero() :: vector:vec(),
	force_relative = vector:zero() :: vector:vec(),
	torque_absolute = vector:zero() :: vector:vec(),
	torque_relative = vector:zero() :: vector:vec(),

	% Intrinsic values (should NOT change during the life of an object)
	mass = 1 :: float(),
	inverse_mass = 1 :: float(),
	inertia_tensor = 1 :: float(),
	inverse_inertia_tensor = 1 :: float(),

	% Purely calculated values (DON'T try to change these from anything other than pre_physics_rk4)
	last_update :: erlang:timestamp(),
	linear_velocity = vector:zero() :: vector:vec(),
	angular_velocity = vector:zero() :: vector:vec(),
	spin = quaternion:identity() :: quaternion:quat()
}).


%% --------------------------------------------------------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------------------------------------------------------

-spec new() -> Physical when
	Physical :: #pre_physical{}.

new() ->
	#pre_physical{}.

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_absolute(AbsForce, Physical) -> UpdatedPhysical when
	AbsForce :: vector:vec(),
	Physical :: #pre_physical{},
	UpdatedPhysical :: #pre_physical{}.

apply_force_absolute(AbsForce, Physical) ->
	Physical#pre_physical{
		force_absolute = vector:add(Physical#pre_physical.force_absolute, AbsForce)
	}.

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_force_relative(RelForce, Physical) -> UpdatedPhysical when
	RelForce :: vector:vec(),
	Physical :: #pre_physical{},
	UpdatedPhysical :: #pre_physical{}.

apply_force_relative(RelForce, Physical) ->
	Physical#pre_physical{
		force_relative = vector:add(Physical#pre_physical.force_relative, RelForce)
	}.

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_absolute(AbsTorque, Physical) -> UpdatedPhysical when
	AbsTorque :: quaternion:vec(),
	Physical :: #pre_physical{},
	UpdatedPhysical :: #pre_physical{}.

apply_torque_absolute(AbsTorque, Physical) ->
	Physical#pre_physical{
		torque_absolute = quaternion:compose(Physical#pre_physical.torque_absolute, AbsTorque)
	}.

%% --------------------------------------------------------------------------------------------------------------------

-spec apply_torque_relative(RelTorque, Physical) -> UpdatedPhysical when
	RelTorque :: quaternion:vec(),
	Physical :: #pre_physical{},
	UpdatedPhysical :: #pre_physical{}.

apply_torque_relative(RelTorque, Physical) ->
	Physical#pre_physical{
		torque_relative = quaternion:compose(Physical#pre_physical.torque_relative, RelTorque)
	}.
