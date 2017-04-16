%%% @doc An entity representing a ship.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_sim_ship).

-behaviour(pre_gen_simulated).

% pre_gen_simulated
-export([init/2, simulate/2]).

-record(state, {
	target_linear_velocity :: {integer(), integer(), integer()},
	target_angular_velocity :: {integer(), integer(), integer()},

	% Intrinsic ship parameters
	linear_target_velocity_scaling :: {integer(), integer(), integer()}, % {sideslip, lift, throttle}
	angular_target_velocity_scaling :: {integer(), integer(), integer()}, %  {pitch, heading, roll}
	max_linear_thrust :: {integer(), integer(), integer()}, % {sideslip, lift, throttle}
	max_angular_thrust :: {integer(), integer(), integer()}, %  {pitch, heading, roll}
	linear_responsiveness :: {integer(), integer(), integer()}, % {sideslip, lift, throttle}
	angular_responsiveness :: {integer(), integer(), integer()}, %  {pitch, heading, roll}

	id :: term(),
	physical :: any()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% pre_gen_simulated
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, InitData) ->
	Defaults = [
		{ target_linear_velocity, {0, 0, 0} },
		{ target_angular_velocity, {0, 0, 0} },

		% Intrinsic ship parameters
		{ linear_target_velocity_scaling, {1200, 1000, 1600} }, % {sideslip, lift, throttle}
		{ angular_target_velocity_scaling, {2, 2, 2} }, %  {pitch, heading, roll}
		{ max_linear_thrust, {600, 500, 800} }, % {sideslip, lift, throttle}
		{ max_angular_thrust, {2, 2, 2} }, %  {pitch, heading, roll}
		{ linear_responsiveness, {3, 3, 3} }, % {sideslip, lift, throttle}
		{ angular_responsiveness, {3, 3, 3} }, %  {pitch, heading, roll}

		{position,
			{
				random:uniform() * 2000 - 100,
				random:uniform() * 2000 + 600,
				random:uniform() * 200
			}
		},
		{orientation,
			quaternion:from_axis_angle(
				vector:unit({
					random:uniform(),
					random:uniform(),
					random:uniform()
				}),
				random:uniform() * 2 * math:pi()
			)
		},

		{ model_def, [{model, <<"Ships/ares">>}]}
	],

	% Set up default model def, and default physical state with random position/orientation
	InitDataWithDefaults = lists:ukeymerge(1, InitData, Defaults),

	Physical = pre_sim_physical:init(EntityID, InitDataWithDefaults),

	#state{
		target_linear_velocity = proplists:get_value(target_linear_velocity, InitData),
		target_angular_velocity = proplists:get_value(target_angular_velocity, InitData),

		% Intrinsic ship parameters
		linear_target_velocity_scaling = proplists:get_value(linear_target_velocity_scaling, InitData),
		angular_target_velocity_scaling = proplists:get_value(angular_target_velocity_scaling, InitData),
		max_linear_thrust = proplists:get_value(max_linear_thrust, InitData),
		max_angular_thrust = proplists:get_value(max_angular_thrust, InitData),
		linear_responsiveness = proplists:get_value(linear_responsiveness, InitData),
		angular_responsiveness = proplists:get_value(angular_responsiveness, InitData),

		id = EntityID,
		physical = Physical
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(Updates, State) ->
	PhysicalUpdates = lists:filtermap(
		fun
			({update, Key, Value}, {Unhandled, State1}) ->
				apply_update(Key, Value, {Unhandled, State1});
			(Other, {Unhandled, State1}) ->
				{[Other | Unhandled], State1}
		end,
		Updates
	),

	{ShipUpdate, State1} = do_flight_control(State),

	{PhysicalUpdate, NewPhysical} = pre_sim_physical:simulate(PhysicalUpdates, State1#state.physical),

	{ShipUpdate ++ PhysicalUpdate, State1#state { physical = NewPhysical }}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------------------------------------------------------

apply_update(target_linear_velocity, Value, {Unhandled, State}) ->
	{Unhandled, State#state { target_linear_velocity = Value }};

apply_update(target_angular_velocity, Value, {Unhandled, State}) ->
	{Unhandled, State#state { target_angular_velocity = Value }};

apply_update(linear_target_velocity_scaling, Value, {Unhandled, State}) ->
	{Unhandled, State#state { linear_target_velocity_scaling = Value }};

apply_update(angular_target_velocity_scaling, Value, {Unhandled, State}) ->
	{Unhandled, State#state { angular_target_velocity_scaling = Value }};

apply_update(max_linear_thrust, Value, {Unhandled, State}) ->
	{Unhandled, State#state { max_linear_thrust = Value }};

apply_update(max_angular_thrust, Value, {Unhandled, State}) ->
	{Unhandled, State#state { max_angular_thrust = Value }};

apply_update(linear_responsiveness, Value, {Unhandled, State}) ->
	{Unhandled, State#state { linear_responsiveness = Value }};

apply_update(angular_responsiveness, Value, {Unhandled, State}) ->
	{Unhandled, State#state { angular_responsiveness = Value }};

apply_update(Key, Value, {Unhandled, State}) ->
	{[{Key, Value} | Unhandled], State}.

%% --------------------------------------------------------------------------------------------------------------------

do_flight_control(State) ->
	Physical = State#state.physical,
	Orientation = pre_sim_physical:get_orientation(Physical),
	PositionVelAbs = pre_sim_physical:get_linear_velocity(Physical),
	AngularVelAbs = pre_sim_physical:get_angular_velocity(Physical),
	OldForce = pre_sim_physical:get_force_relative(Physical),
	OldTorque = pre_sim_physical:get_torque_relative(Physical),

	{TX, TY, TZ} = State#state.target_linear_velocity,
	{TPitch, THeading, TRoll} = State#state.target_angular_velocity,
	{TXScale, TYScale, TZScale} = State#state.linear_target_velocity_scaling,
	{TPitchScale, THeadingScale, TRollScale} = State#state.angular_target_velocity_scaling,
	{MaxXT, MaxYT, MaxZT} = State#state.max_linear_thrust,
	{MaxPitchT, MaxHeadingT, MaxRollT} = State#state.max_angular_thrust,
	{XR, YR, ZR} = State#state.linear_responsiveness,
	{PitchR, HeadingR, RollR} = State#state.angular_responsiveness,

	% Get the ship-relative linear velocity
	{XVel, YVel, ZVel} = quaternion:rotate(PositionVelAbs, quaternion:reciprocal(Orientation)),

	% Get the ship-relative angular velocity
	{PitchVel, HeadingVel, RollVel} = quaternion:rotate(AngularVelAbs, quaternion:reciprocal(Orientation)),

	% Calculate relative force
	Force = {
		calc_thrust(MaxXT, XR, XVel, TXScale * TX),
		calc_thrust(MaxYT, YR, YVel, TYScale * TY),
		calc_thrust(MaxZT, ZR, ZVel, TZScale * TZ)
	},

	% Calculate relative torque
	Torque = {
		calc_thrust(MaxPitchT, PitchR, PitchVel, TPitchScale * TPitch),
		calc_thrust(MaxHeadingT, HeadingR, HeadingVel, THeadingScale * THeading),
		calc_thrust(MaxRollT, RollR, RollVel, TRollScale * TRoll)
	},

	PhysicalChanges = [
		{force_relative, OldForce, Force},
		{torque_relative, OldTorque, Torque}
	],
	PhysicalUpdates = [
		{Key, NewVal}
		|| {Key, OldVal, NewVal} <- PhysicalChanges, OldVal =/= NewVal
	],

	case PhysicalUpdates of
		[] ->
			{[], State};
		_ ->
			% Update physical state
			NewPhysical = pre_sim_physical:apply_update(physical, PhysicalUpdates, Physical#state.physical),

			% Update entity state
			State1 = State#state{
				physical = NewPhysical
			},

			% Create delta update
			Update = [{physical, [{K, vector:vec_to_list(V)} || {K, V} <- PhysicalUpdates]}],

			{Update, State1}
	end.


calc_thrust(MaxTh, Resp, CurVel, TargetVel) ->
	% We might want to go back to limiting this, but only after we modify the code to also zero velocity and
	% acceleration.
	%case abs(CurVel - TargetVel) < 0.0001 of
	%	false ->
	%		% DMToP = Double Max Thrust over Pi.
	%		DMToP = 2 * MaxTh / math:pi(),
	%		DMToP * math:atan((TargetVel - CurVel) * Resp / DMToP);
	%	_ ->
	%	0
	%end.

	% DMToP = Double Max Thrust over Pi.
	DMToP = 2 * MaxTh / math:pi(),
	DMToP * math:atan((TargetVel - CurVel) * Resp / DMToP).
