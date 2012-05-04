%% ------------------------------------------------------------------------
%% @doc Physics: 4th-order Runge-Kutta integration
%%
%% @copyright 2012 David H. Bronke
%% Licensed under the MIT license; see the LICENSE file for details.
%% ------------------------------------------------------------------------
%% This module is loosely based on concepts from the following articles:
%% * http://gafferongames.com/game-physics/integration-basics/
%% * http://gafferongames.com/game-physics/physics-in-3d/
%% ------------------------------------------------------------------------

-module(pre_physics_rk4).

% -------------------------------------------------------------------------

% external api 
-export([simulate/2]).

% -------------------------------------------------------------------------

-include("log.hrl").
%-include("pre_physics.hrl").

-record(physical, {
	% Updated values (assume these change every frame)
	position = {0, 0, 0} :: vector:vec(),
	linear_momentum = {0, 0, 0} :: vector:vec(),
	orientation = {1, 0, 0, 0} :: quaternion:quat(),
	angular_momentum = {0, 0, 0} :: vector:vec(),

	% Input-only values
	force_abs = {0, 0, 0} :: vector:vec(),
	force_rel = {0, 0, 0} :: vector:vec(),
	torque_absolute = {0, 0, 0} :: vector:vec(),
	torque_relative = {0, 0, 0} :: vector:vec(),

	% Purely calculated values (DON'T try to change these externally)
	last_update :: erlang:timestamp(),
	linear_velocity = {0, 0, 0} :: vector:vec(),
	angular_velocity = {0, 0, 0} :: vector:vec(),
	spin = {1, 0, 0, 0} :: quaternion:quat(),

	% Constant values
	mass = 1 :: float(),
	inverse_mass = 1 :: float(),
	inertia_tensor = 1 :: float(),
	inverse_inertia_tensor = 1 :: float()
}).

%% ------------------------------------------------------------------------
%% External API
%% ------------------------------------------------------------------------

-spec evaluate(TimeDelta, Velocity, Force, Spin, Torque, State) -> {Velocity, Force, Spin, Torque, State} when
	TimeDelta :: float(),
	Velocity :: vector:vec(),
	Force :: vector:vec(),
	Spin :: quaternion:quat(),
	Torque :: vector:vec(),
	State :: #physical{}.

evaluate(TimeDelta, Velocity, Force, Spin, Torque, State) ->
	#physical{
		position = Position,
		linear_velocity = InitialVelocity,
		orientation = Orientation,
		angular_momentum = AngularMomentum
	} = State,

	NextPosition = Position + Velocity * TimeDelta,
	NextVelocity = InitialVelocity + Force * TimeDelta,
	NextOrientation = Orientation + Spin * TimeDelta,
	NextAngularMomentum = AngularMomentum + Torque * TimeDelta,

	NextState = State#physical{
		position = NextPosition,
		linear_velocity = NextVelocity,
		orientation = quaternion:unit(NextOrientation),
		angular_momentum = NextAngularMomentum
	},
	%TODO: Should _NextAngularVelocity even be returned?
	{NextSpin, _NextAngularVelocity} = update_state(NextState),

	{NextForce, NextTorque} = forces(TimeDelta, NextState),

	{NextVelocity, NextForce, NextSpin, NextTorque, NextState}.

%% ------------------------------------------------------------------------

-spec forces(TimeDelta, State) -> {Force, Torque} when
	TimeDelta :: float(),
	State :: #physical{},
	Force :: vector:vec(),
	Torque :: vector:vec().

%FIXME: This should be a callback so the behavior can do stuff like target-velocity calculations.
forces(_TimeDelta, State) ->
	#physical{
		force_abs = ForceAbs,
		force_rel = ForceRel,
		orientation = Orientation,
		torque_absolute = TorqueAbs,
		torque_relative = TorqueRel
	} = State,

	Force = vector:add(ForceAbs, quaternion:rotate(ForceRel, Orientation)),
	Torque = vector:add(TorqueAbs, quaternion:rotate(TorqueRel, Orientation)),

	{Force, Torque}.

%% ------------------------------------------------------------------------

-spec update_state(State) -> {Spin, AngularVelocity} when
	State :: #physical{},
	Spin :: quaternion:quat(),
	AngularVelocity :: vector:vec().

update_state(State) ->
	#physical{
		linear_momentum = LinearMomentum,
		orientation = Orientation,
		angular_momentum = AngularMomentum,
		inverse_mass = InverseMass,
		inverse_inertia_tensor = InverseInertia
	} = State,

	LinearVelocity = LinearMomentum * InverseMass,

	AngularVelocity = vector:multiply(InverseInertia, AngularMomentum),
	{AngularVelocityX, AngularVelocityY, AngularVelocityZ} = AngularVelocity,

	Spin = quaternion:multiply(
		0.5,
		quaternion:multiply(
			{0, AngularVelocityX, AngularVelocityY, AngularVelocityZ},
			Orientation
		)
	),
	State#physical{
		linear_velocity = LinearVelocity,
		angular_velocity = AngularVelocity,
		spin = Spin
	}.

%% ------------------------------------------------------------------------

%% @doc Simulate physical movement of the 'physical' object represented by `State`, over the given `TimeDelta`.
simulate(TimeDelta, State) ->
	#physical{
		position = Position,
		linear_velocity = Velocity,
		orientation = Orientation,
		angular_momentum = AngularMomentum
	} = State,
	
	{Velocity1, Force1, Spin1, Torque1, State1} = evaluate(0, 0, 0, 0, 0, State),
	{Velocity2, Force2, Spin2, Torque2, State2} = evaluate(TimeDelta * 0.5, Velocity1, Force1, Spin1, Torque1, State1),
	{Velocity3, Force3, Spin3, Torque3, State3} = evaluate(TimeDelta * 0.5, Velocity2, Force2, Spin2, Torque2, State2),
	{Velocity4, Force4, Spin4, Torque4, State4} = evaluate(TimeDelta, Velocity3, Force3, Spin3, Torque3, State3),

	NewVelocity = 1.0 / 6.0 * (Velocity1 + 2.0 * (Velocity2 + Velocity3) + Velocity4),
	NewAcceleration = 1.0 / 6.0 * (Force1 + 2.0 * (Force2 + Force3) + Force4),
	NewSpin = 1.0 / 6.0 * (Spin1 + 2.0 * (Spin2 + Spin3) + Spin4),
	NewTorque = 1.0 / 6.0 * (Torque1 + 2.0 * (Torque2 + Torque3) + Torque4),

	State4#physical{
		position = Position + NewVelocity * TimeDelta,
		linear_velocity = Velocity + NewAcceleration * TimeDelta,
		orientation = quaternion:unit(Orientation + NewSpin * TimeDelta),
		angular_momentum = AngularMomentum + NewTorque * TimeDelta
	}.
