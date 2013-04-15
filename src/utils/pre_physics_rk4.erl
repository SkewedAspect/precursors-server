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
-export([simulate/1, default_physical/0, to_proplist/1, diff_to_proplist/2, from_proplist/1,
	update_from_proplist/2, get_prop/2]).

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

%% @doc Simulate physical movement of the 'physical' object represented by `InitialPhysical`, over the time since that
%% object's 'last_update' timestamp.
simulate(InitialPhysical) ->
	LastUpdate = InitialPhysical#physical.last_update,
	ThisUpdate = os:timestamp(),
	NewPhysical = simulate(timer:now_diff(ThisUpdate, LastUpdate) / 1000000, InitialPhysical),
	NewPhysical#physical {
		last_update = ThisUpdate
	}.

%% @doc Simulate physical movement of the 'physical' object represented by `InitialPhysical`, over the given
%% `TimeDelta`.
simulate(TimeDelta, InitialPhysical) ->
	simulate_internal(TimeDelta, InitialPhysical).

%% ------------------------------------------------------------------------

%% @doc Create a new 'physical' record, with the 'last_update' timestamp set to the current time.
default_physical() ->
	#physical{last_update = os:timestamp()}.

%% ------------------------------------------------------------------------

to_proplist(Physical) ->
	[
		{position, vector:vec_to_list(Physical#physical.position)},
		{linear_momentum, vector:vec_to_list(Physical#physical.linear_momentum)},
		{orientation, quaternion:quat_to_list(Physical#physical.orientation)},
		{angular_momentum, vector:vec_to_list(Physical#physical.angular_momentum)},

		{force_absolute, vector:vec_to_list(Physical#physical.force_absolute)},
		{force_relative, vector:vec_to_list(Physical#physical.force_relative)},
		{torque_absolute, vector:vec_to_list(Physical#physical.torque_absolute)},
		{torque_relative, vector:vec_to_list(Physical#physical.torque_relative)},

		%{last_update, pre_channel_entity:generate_timestamp(Physical#physical.last_update)},
		{linear_velocity, vector:vec_to_list(Physical#physical.linear_velocity)},
		{angular_velocity, vector:vec_to_list(Physical#physical.angular_velocity)},
		{spin, quaternion:quat_to_list(Physical#physical.spin)},

		{mass, Physical#physical.mass},
		{inverse_mass, Physical#physical.inverse_mass},
		{inertia_tensor, Physical#physical.inertia_tensor},
		{inverse_inertia_tensor, Physical#physical.inverse_inertia_tensor}
	].

%% ------------------------------------------------------------------------

diff_to_proplist(OldPhysical, NewPhysical) ->
	filter_diff_list([
		{position, NewPhysical#physical.position, OldPhysical#physical.position},
		{linear_momentum, NewPhysical#physical.linear_momentum, OldPhysical#physical.linear_momentum},
		{orientation, NewPhysical#physical.orientation, OldPhysical#physical.orientation},
		{angular_momentum, NewPhysical#physical.angular_momentum, OldPhysical#physical.angular_momentum},

		{force_absolute, NewPhysical#physical.force_absolute, OldPhysical#physical.force_absolute},
		{force_relative, NewPhysical#physical.force_relative, OldPhysical#physical.force_relative},
		{torque_absolute, NewPhysical#physical.torque_absolute, OldPhysical#physical.torque_absolute},
		{torque_relative, NewPhysical#physical.torque_relative, OldPhysical#physical.torque_relative},

		%{last_update, NewPhysical#physical.last_update, OldPhysical#physical.last_update},
		{linear_velocity, NewPhysical#physical.linear_velocity, OldPhysical#physical.linear_velocity},
		{angular_velocity, NewPhysical#physical.angular_velocity, OldPhysical#physical.angular_velocity},
		{spin, NewPhysical#physical.spin, OldPhysical#physical.spin},

		{mass, NewPhysical#physical.mass, OldPhysical#physical.mass},
		{inverse_mass, NewPhysical#physical.inverse_mass, OldPhysical#physical.inverse_mass},
		{inertia_tensor, NewPhysical#physical.inertia_tensor, OldPhysical#physical.inertia_tensor},
		{inverse_inertia_tensor,
			NewPhysical#physical.inverse_inertia_tensor, OldPhysical#physical.inverse_inertia_tensor}
	]).

filter_diff_list([{_Key, OldAndNewValue, OldAndNewValue} | Rest]) ->
	filter_diff_list(Rest);

%filter_diff_list([{last_update, NewValue, _OldValue} | Rest]) ->
%	[{last_update, pre_channel_entity:generate_timestamp(NewValue)} | filter_diff_list(Rest)];

filter_diff_list([{Key, {_, _, _, _} = NewValue, _OldValue} | Rest]) ->
	[{Key, quaternion:quat_to_list(NewValue)} | filter_diff_list(Rest)];

filter_diff_list([{Key, {_, _, _} = NewValue, _OldValue} | Rest]) ->
	[{Key, vector:vec_to_list(NewValue)} | filter_diff_list(Rest)];

filter_diff_list([{Key, NewValue, _OldValue} | Rest]) ->
	[{Key, NewValue} | filter_diff_list(Rest)];

filter_diff_list([]) ->
	[].

%% ------------------------------------------------------------------------

from_proplist(PhysicalProplist) ->
	update_from_proplist(#physical{}, PhysicalProplist).

%% ------------------------------------------------------------------------

update_from_proplist(Physical, []) ->
	Physical;

update_from_proplist(Physical, [{}]) ->
	Physical;

update_from_proplist(Physical, [{position, Val} | Rest]) ->
	update_from_proplist(Physical#physical{position = Val}, Rest);
update_from_proplist(Physical, [{linear_momentum, Val} | Rest]) ->
	update_from_proplist(Physical#physical{linear_momentum = Val}, Rest);
update_from_proplist(Physical, [{orientation, Val} | Rest]) ->
	update_from_proplist(Physical#physical{orientation = Val}, Rest);
update_from_proplist(Physical, [{angular_momentum, Val} | Rest]) ->
	update_from_proplist(Physical#physical{angular_momentum = Val}, Rest);

update_from_proplist(Physical, [{force_absolute, Val} | Rest]) ->
	update_from_proplist(Physical#physical{force_absolute = Val}, Rest);
update_from_proplist(Physical, [{force_relative, Val} | Rest]) ->
	update_from_proplist(Physical#physical{force_relative = Val}, Rest);
update_from_proplist(Physical, [{torque_absolute, Val} | Rest]) ->
	update_from_proplist(Physical#physical{torque_absolute = Val}, Rest);
update_from_proplist(Physical, [{torque_relative, Val} | Rest]) ->
	update_from_proplist(Physical#physical{torque_relative = Val}, Rest);

update_from_proplist(Physical, [{last_update, Val} | Rest]) ->
	update_from_proplist(Physical#physical{last_update = Val}, Rest);
update_from_proplist(Physical, [{linear_velocity, Val} | Rest]) ->
	update_from_proplist(Physical#physical{linear_velocity = Val}, Rest);
update_from_proplist(Physical, [{angular_velocity, Val} | Rest]) ->
	update_from_proplist(Physical#physical{angular_velocity = Val}, Rest);
update_from_proplist(Physical, [{spin, Val} | Rest]) ->
	update_from_proplist(Physical#physical{spin = Val}, Rest);

update_from_proplist(Physical, [{mass, Val} | Rest]) ->
	update_from_proplist(Physical#physical{mass = Val}, Rest);
update_from_proplist(Physical, [{inverse_mass, Val} | Rest]) ->
	update_from_proplist(Physical#physical{inverse_mass = Val}, Rest);
update_from_proplist(Physical, [{inertia_tensor, Val} | Rest]) ->
	update_from_proplist(Physical#physical{inertia_tensor = Val}, Rest);
update_from_proplist(Physical, [{inverse_inertia_tensor, Val} | Rest]) ->
	update_from_proplist(Physical#physical{inverse_inertia_tensor = Val}, Rest).

%% ------------------------------------------------------------------------

get_prop(position, Physical) ->
	Physical#physical.position;
get_prop(linear_momentum, Physical) ->
	Physical#physical.linear_momentum;
get_prop(orientation, Physical) ->
	Physical#physical.orientation;
get_prop(angular_momentum, Physical) ->
	Physical#physical.angular_momentum;

get_prop(force_absolute, Physical) ->
	Physical#physical.force_absolute;
get_prop(force_relative, Physical) ->
	Physical#physical.force_relative;
get_prop(torque_absolute, Physical) ->
	Physical#physical.torque_absolute;
get_prop(torque_relative, Physical) ->
	Physical#physical.torque_relative;

get_prop(last_update, Physical) ->
	Physical#physical.last_update;
get_prop(linear_velocity, Physical) ->
	Physical#physical.linear_velocity;
get_prop(angular_velocity, Physical) ->
	Physical#physical.angular_velocity;
get_prop(spin, Physical) ->
	Physical#physical.spin;

get_prop(mass, Physical) ->
	Physical#physical.mass;
get_prop(inverse_mass, Physical) ->
	Physical#physical.inverse_mass;
get_prop(inertia_tensor, Physical) ->
	Physical#physical.inertia_tensor;
get_prop(inverse_inertia_tensor, Physical) ->
	Physical#physical.inverse_inertia_tensor.

%% ------------------------------------------------------------------------
%% Internal Helpers
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

	State5 = State4#physical{
		position = vector:add(Position, vector:multiply(TimeDelta, NewVelocity)),
		linear_velocity = vector:add(Velocity, vector:multiply(TimeDelta, NewAcceleration)),
		orientation = quaternion:unit({
			OrientW + TimeDelta * NewSpinW,
			OrientX + TimeDelta * NewSpinX,
			OrientY + TimeDelta * NewSpinY,
			OrientZ + TimeDelta * NewSpinZ
		}),
		angular_momentum = vector:add(AngularMomentum, vector:multiply(TimeDelta, NewTorque))
	},

	% Debug Simulation
	%?warning("Before simulate_internal: ~p", [State]),
	%?warning("After simulate_internal: ~p", [State5]),

	State5.
