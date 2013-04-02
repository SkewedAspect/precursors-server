%%% @doc Physics: 4th-order Runge-Kutta integration
%%%
%%% @copyright 2012 David H. Bronke
%%% Licensed under the MIT license; see the LICENSE file for details.
%%% -------------------------------------------------------------------------------------------------------------------
%%% This module is loosely based on concepts from the following articles:
%%% * http://gafferongames.com/game-physics/integration-basics/
%%% * http://gafferongames.com/game-physics/physics-in-3d/
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_physics_rk4).

%% --------------------------------------------------------------------------------------------------------------------

% external api
-export([simulate/2]).

%% --------------------------------------------------------------------------------------------------------------------

-include("log.hrl").

%% --------------------------------------------------------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------------------------------------------------------

-spec evaluate(TimeDelta, Velocity, Force, Spin, Torque, State) -> {Velocity, Force, Spin, Torque, State} when
	TimeDelta :: float(),
	Velocity :: vector:vec(),
	Force :: vector:vec(),
	Spin :: quaternion:quat(),
	Torque :: vector:vec(),
	State :: dict().

evaluate(TimeDelta, Velocity, Force, Spin, Torque, State) ->

	Position = dict:fetch(position, State),
	InitialVelocity = dict:fetch(linear_velocity, State),
	Orientation = dict:fetch(orientation, State),
	AngularMomentum = dict:fetch(angular_momentum ,State),

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

	State1 = dict:store(position, NextPosition, State),
	State2 = dict:store(linear_velocity, NextVelocity, State1),
	State3 = dict:store(orientation, quaternion:unit(NextOrientation), State2),
	State4 = dict:store(angular_momentum, NextAngularMomentum, State3),

	%TODO: Should _NextAngularVelocity even be returned?
	{NextSpin, State5} = update_state(State4),

	{NextForce, NextTorque} = forces(TimeDelta, State5),

	{NextVelocity, NextForce, NextSpin, NextTorque, State5}.

%% --------------------------------------------------------------------------------------------------------------------

-spec forces(TimeDelta, State) -> {Force, Torque} when
	TimeDelta :: float(),
	State :: dict(),
	Force :: vector:vec(),
	Torque :: vector:vec().

%FIXME: This should be a callback so the behavior can do stuff like target-velocity calculations.
forces(_TimeDelta, State) ->
	ForceAbs = dict:fetch(force_absolute, State),
	ForceRel = dict:fetch(force_relative, State),
	Orientation = dict:fetch(orientation, State),
	TorqueAbs = dict:fetch(torque_absolute, State),
	TorqueRel = dict:fetch(torque_relative, State),

	Force = vector:add(ForceAbs, quaternion:rotate(ForceRel, Orientation)),
	Torque = vector:add(TorqueAbs, quaternion:rotate(TorqueRel, Orientation)),

	{Force, Torque}.

%% --------------------------------------------------------------------------------------------------------------------

-spec update_state(State) -> {Spin, AngularVelocity} when
	State :: dict(),
	Spin :: quaternion:quat(),
	AngularVelocity :: vector:vec().

update_state(State) ->
	LinearMomentum = dict:fetch(linear_momentum, State),
	Orientation = dict:fetch(orientation, State),
	AngularMomentum = dict:fetch(angular_momentum, State),
	InverseMass = dict:fetch(inverse_mass, State),
	InverseInertia = dict:fetch(inverse_inertia_tensor, State),

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

	NewState = dict:store(linear_velocity, LinearVelocity, State),
	NewState1 = dict:store(angular_velocity, AngularVelocity, NewState),
	NewState2 = dict:store(spin, Spin, NewState1),

	{Spin, NewState2}.

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Simulate physical movement of the 'physical' object represented by `State`, over the given `TimeDelta`.

simulate(TimeDelta, State) ->
	Position = dict:fetch(position, State),
	Velocity = dict:fetch(linear_velocity, State),
	{OrientW, OrientX, OrientY, OrientZ} = dict:fetch(orientation, State),
	AngularMomentum = dict:fetch(angular_momentum, State),

	{Velocity1, Force1, Spin1, Torque1, State1} = evaluate(0, {0, 0, 0}, {0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0}, State),
	{Velocity2, Force2, Spin2, Torque2, State2} = evaluate(TimeDelta * 0.5, Velocity1, Force1, Spin1, Torque1, State1),
	{Velocity3, Force3, Spin3, Torque3, State3} = evaluate(TimeDelta * 0.5, Velocity2, Force2, Spin2, Torque2, State2),
	{Velocity4, Force4, Spin4, Torque4, State4} = evaluate(TimeDelta, Velocity3, Force3, Spin3, Torque3, State3),

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

	NewState = dict:from_list([
		{ position, vector:add(Position, vector:multiply(TimeDelta, NewVelocity)) },
		{ linear_velocity, vector:add(Velocity, vector:multiply(TimeDelta, NewAcceleration)) },
		{
			orientation, quaternion:unit({
				OrientW + TimeDelta * NewSpinW,
				OrientX + TimeDelta * NewSpinX,
				OrientY + TimeDelta * NewSpinY,
				OrientZ + TimeDelta * NewSpinZ
			})
		},
		{ angular_momentum, vector:add(AngularMomentum, vector:multiply(TimeDelta, NewTorque)) }
	]),

	% Merge in State 4 and the calculated state. Note: This might not be required, but it protects us from ever loosing state unexpectedly.
	dict:merge(fun(_Key, _InitialVal, NewVal) ->
		NewVal
	end, State4, NewState).
