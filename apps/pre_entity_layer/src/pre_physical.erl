%%% @doc The physical state of an object.
%%%
%%% @copyright 2012 David H. Bronke
%%% Licensed under the MIT license; see the LICENSE file for details.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_physical).

-compile([{parse_transform, rec2json}]).

% API
-export([
	new/0,
	apply_force_absolute/2,
	apply_force_relative/2,
	apply_torque_absolute/2,
	apply_torque_relative/2,
	simulate/2
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

%% --------------------------------------------------------------------------------------------------------------------
%% Physics Simulation
%%
%% This portion of the module is loosely based on concepts from the following articles:
%% * http://gafferongames.com/game-physics/integration-basics/
%% * http://gafferongames.com/game-physics/physics-in-3d/
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Simulate physical movement of `Physical` over the period given by `TimestepUsec`.
-spec simulate(TimestepUsec, Physical) -> UpdatedPhysical when
	TimestepUsec :: float(),
	Physical :: #pre_physical{},
	UpdatedPhysical :: #pre_physical{}.

simulate(TimestepUsec, Physical) ->
	TimestampSec = TimestepUsec / 1000000,
	#pre_physical{
		position = Position,
		linear_velocity = Velocity,
		orientation = {OrientW, OrientX, OrientY, OrientZ},
		angular_momentum = AngularMomentum
	} = Physical,

	{Velocity1, Force1, Spin1, Torque1, Physical1} = sim_evaluate(0, vector:zero(), vector:zero(), vector:zero(), vector:zero(), Physical),
	{Velocity2, Force2, Spin2, Torque2, Physical2} = sim_evaluate(TimestampSec * 0.5, Velocity1, Force1, Spin1, Torque1, Physical1),
	{Velocity3, Force3, Spin3, Torque3, Physical3} = sim_evaluate(TimestampSec * 0.5, Velocity2, Force2, Spin2, Torque2, Physical2),
	{Velocity4, Force4, Spin4, Torque4, Physical4} = sim_evaluate(TimestampSec, Velocity3, Force3, Spin3, Torque3, Physical3),

	NewVelocity = vector:multiply(1.0 / 6.0, vector:add(Velocity1, vector:multiply(2.0, vector:add(Velocity2, Velocity3)), Velocity4)),
	NewAcceleration = vector:multiply(1.0 / 6.0, vector:add(Force1, vector:multiply(2.0, vector:add(Force2, Force3)), Force4)),

	{Spin1W, Spin1X, Spin1Y, Spin1Z} = Spin1,
	{Spin2W, Spin2X, Spin2Y, Spin2Z} = Spin2,
	{Spin3W, Spin3X, Spin3Y, Spin3Z} = Spin3,
	{Spin4W, Spin4X, Spin4Y, Spin4Z} = Spin4,
	NewSpinW = 1.0 / 6.0 * (Spin1W + 2.0 * (Spin2W + Spin3W) + Spin4W),
	NewSpinX = 1.0 / 6.0 * (Spin1X + 2.0 * (Spin2X + Spin3X) + Spin4X),
	NewSpinY = 1.0 / 6.0 * (Spin1Y + 2.0 * (Spin2Y + Spin3Y) + Spin4Y),
	NewSpinZ = 1.0 / 6.0 * (Spin1Z + 2.0 * (Spin2Z + Spin3Z) + Spin4Z),

	NewTorque = vector:multiply(1.0 / 6.0, vector:add(Torque1, vector:multiply(2.0, vector:add(Torque2, Torque3)), Torque4)),

	Physical5 = Physical4#pre_physical{
		position = vector:add(Position, vector:multiply(TimestampSec, NewVelocity)),
		linear_velocity = vector:add(Velocity, vector:multiply(TimestampSec, NewAcceleration)),
		orientation = quaternion:unit({
			OrientW + TimestampSec * NewSpinW,
			OrientX + TimestampSec * NewSpinX,
			OrientY + TimestampSec * NewSpinY,
			OrientZ + TimestampSec * NewSpinZ
		}),
		angular_momentum = vector:add(AngularMomentum, vector:multiply(TimestampSec, NewTorque))
	},

	% Debug Simulation
	%?warning("Before `simulate`: ~p", [Physical]),
	%?warning("After `simulate`: ~p", [Physical5]),

	Physical5.

%% --------------------------------------------------------------------------------------------------------------------

-spec sim_evaluate(TimeDelta, Velocity, Force, Spin, Torque, State) -> {Velocity, Force, Spin, Torque, State} when
	TimeDelta :: float(),
	Velocity :: vector:vec(),
	Force :: vector:vec(),
	Spin :: quaternion:quat(),
	Torque :: vector:vec(),
	State :: #pre_physical{}.

sim_evaluate(TimeDelta, Velocity, Force, Spin, Torque, State) ->
	#pre_physical{
		position = Position,
		linear_velocity = InitialVelocity,
		orientation = Orientation,
		angular_momentum = AngularMomentum
	} = State,

	{SpinW, SpinX, SpinY, SpinZ} = Spin,
	{OrientW, OrientX, OrientY, OrientZ} = Orientation,

	NextPosition = vector:add(Position, vector:multiply(TimeDelta, Velocity)),
	NextVelocity = vector:add(InitialVelocity, vector:multiply(TimeDelta, Force)),
	%XXX: I have no idea if/how this works, but it's what the example code did...
	NextOrientation = {
		OrientW + SpinW * TimeDelta,
		OrientX + SpinX * TimeDelta,
		OrientY + SpinY * TimeDelta,
		OrientZ + SpinZ * TimeDelta
	},
	NextAngularMomentum = vector:add(AngularMomentum, vector:multiply(TimeDelta, Torque)),

	State1 = State#pre_physical{
		position = NextPosition,
		linear_velocity = NextVelocity,
		orientation = quaternion:unit(NextOrientation),
		angular_momentum = NextAngularMomentum
	},
	%TODO: Should _NextAngularVelocity even be returned?
	{NextSpin, State2} = sim_update_state(State1),

	{NextForce, NextTorque} = sim_forces(TimeDelta, State2),

	{NextVelocity, NextForce, NextSpin, NextTorque, State2}.

%% --------------------------------------------------------------------------------------------------------------------

-spec sim_update_state(State) -> {Spin, AngularVelocity} when
	State :: #pre_physical{},
	Spin :: quaternion:quat(),
	AngularVelocity :: vector:vec().

sim_update_state(State) ->
	#pre_physical{
		linear_momentum = LinearMomentum,
		orientation = Orientation,
		angular_momentum = AngularMomentum,
		inverse_mass = InverseMass,
		inverse_inertia_tensor = InverseInertia
	} = State,

	LinearVelocity = vector:multiply(InverseMass, LinearMomentum),

	AngularVelocity = vector:multiply(InverseInertia, AngularMomentum),
	{AngularVelocityX, AngularVelocityY, AngularVelocityZ} = AngularVelocity,

	Spin = quaternion:multiply(
		0.5,
		quaternion:multiply(
			{0, AngularVelocityX, AngularVelocityY, AngularVelocityZ},
			Orientation
		)
	),
	NewState = State#pre_physical{
		linear_velocity = LinearVelocity,
		angular_velocity = AngularVelocity,
		spin = Spin
	},
	{Spin, NewState}.

%% --------------------------------------------------------------------------------------------------------------------

-spec sim_forces(TimeDelta, State) -> {Force, Torque} when
	TimeDelta :: float(),
	State :: #pre_physical{},
	Force :: vector:vec(),
	Torque :: vector:vec().

sim_forces(_TimeDelta, State) ->
	#pre_physical{
		force_absolute = ForceAbs,
		force_relative = ForceRel,
		orientation = Orientation,
		torque_absolute = TorqueAbs,
		torque_relative = TorqueRel
	} = State,

	Force = vector:add(ForceAbs, quaternion:rotate(ForceRel, Orientation)),
	Torque = vector:add(TorqueAbs, quaternion:rotate(TorqueRel, Orientation)),

	{Force, Torque}.
