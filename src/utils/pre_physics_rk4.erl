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

% -------------------------------------------------------------------------

-record(physical, {
        % Updated values (assume these change every frame)
        position = {0, 0, 0} :: vector:vec(),
        linear_momentum = {0, 0, 0} :: vector:vec(),
        orientation = {1, 0, 0, 0} :: quaternion:quat(),
        angular_momentum = {0, 0, 0} :: vector:vec(),

        % Input-only values
        force_absolute = {0, 0, 0} :: vector:vec(),
        force_relative = {0, 0, 0} :: vector:vec(),
        torque_absolute = {0, 0, 0} :: vector:vec(),
        torque_relative = {0, 0, 0} :: vector:vec(),

        % Purely calculated values (DON'T try to change these externally)
        last_update :: erlang:timestamp(),
        linear_velocity = {0, 0, 0} :: vector:vec(),
        angular_velocity = {0, 0, 0} :: vector:vec(),
        spin = {1, 0, 0, 0} :: quaternion:quat(),

        % Intrinsic values (should NOT change during the life of an object)
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

	State1 = State#physical{
		position = NextPosition,
		linear_velocity = NextVelocity,
		orientation = quaternion:unit(NextOrientation),
		angular_momentum = NextAngularMomentum
	},
	%TODO: Should _NextAngularVelocity even be returned?
	{NextSpin, State2} = update_state(State1),

	{NextForce, NextTorque} = forces(TimeDelta, State2),

	{NextVelocity, NextForce, NextSpin, NextTorque, State2}.

%% ------------------------------------------------------------------------

-spec forces(TimeDelta, State) -> {Force, Torque} when
	TimeDelta :: float(),
	State :: #physical{},
	Force :: vector:vec(),
	Torque :: vector:vec().

%FIXME: This should be a callback so the behavior can do stuff like target-velocity calculations.
forces(_TimeDelta, State) ->
	#physical{
		force_absolute = ForceAbs,
		force_relative = ForceRel,
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
	NewState = State#physical{
		linear_velocity = LinearVelocity,
		angular_velocity = AngularVelocity,
		spin = Spin
	},
	{Spin, NewState}.

%% ------------------------------------------------------------------------

%% @doc Simulate physical movement of the 'physical' object represented by `State`, over the given `TimeDelta`.
simulate(TimeDelta, InitialPhysical) ->

	Physical = #physical{
			position = dict:fetch(position, InitialPhysical),
			linear_momentum = dict:fetch(linear_momentum, InitialPhysical),
			orientation = dict:fetch(orientation, InitialPhysical),
			angular_momentum = dict:fetch(angular_momentum, InitialPhysical),

			force_absolute = dict:fetch(force_absolute, InitialPhysical),
			force_relative = dict:fetch(force_relative, InitialPhysical),
			torque_absolute = dict:fetch(torque_absolute, InitialPhysical),
			torque_relative = dict:fetch(torque_relative, InitialPhysical),

			last_update = dict:fetch(last_update, InitialPhysical),
			linear_velocity = dict:fetch(linear_velocity, InitialPhysical),
			angular_velocity = dict:fetch(angular_velocity, InitialPhysical),
			spin = dict:fetch(spin, InitialPhysical),

			mass = dict:fetch(mass, InitialPhysical),
			inverse_mass = dict:fetch(inverse_mass, InitialPhysical),
			inertia_tensor = dict:fetch(inertia_tensor, InitialPhysical),
			inverse_inertia_tensor = dict:fetch(inverse_inertia_tensor, InitialPhysical)
	},

	NewPhysical = simulate_internal(TimeDelta, Physical),

	dict:from_list([
			{ position, NewPhysical#physical.position },
			{ linear_momentum, NewPhysical#physical.linear_momentum },
			{ orientation, NewPhysical#physical.orientation },
			{ angular_momentum, NewPhysical#physical.angular_momentum },

			{ force_absolute, NewPhysical#physical.force_absolute },
			{ force_relative, NewPhysical#physical.force_relative },
			{ torque_absolute, NewPhysical#physical.torque_absolute },
			{ torque_relative, NewPhysical#physical.torque_relative },

			{ last_update, NewPhysical#physical.last_update },
			{ linear_velocity, NewPhysical#physical.linear_velocity },
			{ angular_velocity, NewPhysical#physical.angular_velocity },
			{ spin, NewPhysical#physical.spin },

			{ mass, NewPhysical#physical.mass },
			{ inverse_mass, NewPhysical#physical.inverse_mass },
			{ inertia_tensor, NewPhysical#physical.inertia_tensor },
			{ inverse_inertia_tensor, NewPhysical#physical.inverse_inertia_tensor }
	]).

%% ------------------------------------------------------------------------

simulate_internal(TimeDelta, State) ->
	#physical{
		position = Position,
		linear_velocity = Velocity,
		orientation = {OrientW, OrientX, OrientY, OrientZ},
		angular_momentum = AngularMomentum
	} = State,

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

	State4#physical{
		position = vector:add(Position, vector:multiply(TimeDelta, NewVelocity)),
		linear_velocity = vector:add(Velocity, vector:multiply(TimeDelta, NewAcceleration)),
		orientation = quaternion:unit({
			OrientW + TimeDelta * NewSpinW,
			OrientX + TimeDelta * NewSpinX,
			OrientY + TimeDelta * NewSpinY,
			OrientZ + TimeDelta * NewSpinZ
		}),
		angular_momentum = vector:add(AngularMomentum, vector:multiply(TimeDelta, NewTorque))
	}.
