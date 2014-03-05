%%% @doc An entity representing a ship.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_ship).

-behaviour(entity_controller).

% entity_controller
-export([init/3, simulate/2, get_full_state/2, client_request/6, client_event/5, entity_event/4, apply_update/4]).

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
	controller :: module(),
	physical :: any()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, Controller, InitData) ->
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

	Physical = entity_physical:init(EntityID, Controller, InitDataWithDefaults),

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
		controller = Controller,
		physical = Physical
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(Controller, State) ->
	{ShipUpdate, State1} = do_flight_control(State),
	{PhysicalUpdate, NewPhysical} = entity_physical:simulate(Controller, State1#state.physical),
	{ShipUpdate ++ PhysicalUpdate, State1#state { physical = NewPhysical }}.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(Controller, State) ->
	{_, PhysicalFullState} = entity_physical:get_full_state(Controller, State#state.physical),

	{<<"Ship">>, [
		{ship, [
			{target_linear_velocity, State#state.target_linear_velocity},
			{target_angular_velocity, State#state.target_angular_velocity},

			% Intrinsic ship parameters
			{linear_target_velocity_scaling, State#state.linear_target_velocity_scaling},
			{angular_target_velocity_scaling, State#state.angular_target_velocity_scaling},
			{max_linear_thrust, State#state.max_linear_thrust},
			{max_angular_thrust, State#state.max_angular_thrust},
			{linear_responsiveness, State#state.linear_responsiveness},
			{angular_responsiveness, State#state.angular_responsiveness}
		]}
		| PhysicalFullState
	]}.

%% --------------------------------------------------------------------------------------------------------------------

client_request(input, <<"command">>, _RequestID, Request, _Controller, State) ->
	handle_input_command(Request, State);

client_request(Channel, RequestType, RequestID, Request, Controller, State) ->
	{Update, NewPhysical} = entity_physical:client_request(Channel, RequestType, RequestID, Request, Controller, State#state.physical),
	{Update, State#state { physical = NewPhysical }}.

%% --------------------------------------------------------------------------------------------------------------------

client_event(input, <<"command">>, Event, _Controller, State) ->
	{_Response, Update, State1} = handle_input_command(Event, State),
	{Update, State1};

client_event(Channel, EventType, Event, Controller, State) ->
	{Update, NewPhysical} = entity_physical:client_event(Channel, EventType, Event, Controller, State#state.physical),
	{Update, State#state { physical = NewPhysical }}.

%% --------------------------------------------------------------------------------------------------------------------

entity_event(Event, From, Controller, State) ->
	{Update, NewPhysical} = entity_physical:client_event(Event, From, Controller, State#state.physical),
	{Update, State#state { physical = NewPhysical }}.

%% --------------------------------------------------------------------------------------------------------------------

apply_update(ship, [], _Controller, State) ->
	{[], State};

apply_update(ship, [{SubKey, NewValue} | Rest], Controller, State) ->
	{Update, State1} = apply_update(ship, Rest, Controller, State),

	State2 = case SubKey of
		target_linear_velocity -> State1#state { target_linear_velocity = NewValue };
		target_angular_velocity -> State1#state { target_angular_velocity = NewValue };

		% Intrinsic ship parameters
		linear_target_velocity_scaling -> State1#state { linear_target_velocity_scaling = NewValue };
		angular_target_velocity_scaling -> State1#state { angular_target_velocity_scaling = NewValue };
		max_linear_thrust -> State1#state { max_linear_thrust = NewValue };
		max_angular_thrust -> State1#state { max_angular_thrust = NewValue };
		linear_responsiveness -> State1#state { linear_responsiveness = NewValue };
		angular_responsiveness -> State1#state { angular_responsiveness = NewValue };
		SubKey -> lager:warning("Ignoring unrecognized update for subkey ~p of base! (value: ~p)", [SubKey, NewValue])
	end,

	{Update, State2};

apply_update(Key, SubKeys, Controller, State) ->
	{Update, NewPhysical} = entity_physical:apply_update(Key, SubKeys, Controller, State#state.physical),
	{Update, State#state { physical = NewPhysical }}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------------------------------------------------------

handle_input_command([{_, _} | _] = RawCommand, State) ->
	Command = proplists:get_value(name, RawCommand),
	Args = proplists:get_value(args, RawCommand),
	KWArgs = proplists:get_value(kwargs, RawCommand),
	handle_input_command(Command, Args, KWArgs, State).

%% --------------------------------------------------------------------------------------------------------------------

% Positional target velocity
handle_input_command(<<"sideslip">>, [TargetVel], _KWArgs, State) ->
	set_target_linear_velocity({TargetVel, undefined, undefined}, State);

handle_input_command(<<"lift">>, [TargetVel], _KWArgs, State) ->
	set_target_linear_velocity({undefined, TargetVel, undefined}, State);

handle_input_command(<<"throttle">>, [TargetVel], _KWArgs, State) ->
	set_target_linear_velocity({undefined, undefined, -TargetVel}, State);

% Rotational target velocity
handle_input_command(<<"pitch">>, [TargetVel], _KWArgs, State) ->
	set_target_angular_velocity({TargetVel, undefined, undefined}, State);

handle_input_command(<<"heading">>, [TargetVel], _KWArgs, State) ->
	set_target_angular_velocity({undefined, TargetVel, undefined}, State);

handle_input_command(<<"roll">>, [TargetVel], _KWArgs, State) ->
	set_target_angular_velocity({undefined, undefined, TargetVel}, State);

% Catch-all
handle_input_command(Command, Args, KWArgs, State) ->
	lager:info("Got unrecognized input command: ~p, ~p, ~p", [Command, Args, KWArgs]),
	Response = [
		{confirm, false},
		{reason, <<"Unrecognized input command \"", Command/binary, "\"!">>}
	],
	{Response, [], State}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_angular_velocity(RequestedAngVel, State) ->
	CurrentAngVel = State#state.target_angular_velocity,
	Response = [{confirm, true}],
	{Update, State1} = set_target_angular_velocity(CurrentAngVel, update_vector(RequestedAngVel, CurrentAngVel), State),
	{Response, Update, State1}.

set_target_angular_velocity(CurAndNewAngVel, CurAndNewAngVel, State) ->
	% No change; confirm, but don't produce an update.
	{[], State};

set_target_angular_velocity(_CurrentAngVel, NewAngVel, State) ->
	%?debug("Setting orientation velocity to ~p.", [NewAngVel]),
	% Update the target angular velocity
	State1 = State#state{
		target_angular_velocity = NewAngVel
	},

	% Build update
	Update = [{ship, [{target_angular_velocity, vector:vec_to_list(NewAngVel)}]}],
	{Update, State1}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_linear_velocity(RequestedLinVel, State) ->
	CurrentLinVel = State#state.target_linear_velocity,
	Response = [{confirm, true}],
	{Update, State1} = set_target_linear_velocity(CurrentLinVel, update_vector(RequestedLinVel, CurrentLinVel), State),
	{Response, Update, State1}.

set_target_linear_velocity(CurAndNewLinVel, CurAndNewLinVel, State) ->
	% No change; confirm, but don't produce an update.
	{[], State};

set_target_linear_velocity(_CurrentLinVel, NewLinVel, State) ->
	%?debug("Setting position velocity to ~p.", [NewLinVel]),
	% Update the target linear velocity
	State1 = State#state{
		target_linear_velocity = NewLinVel
	},

	% Build update
	Update = [{ship, [{target_linear_velocity, vector:vec_to_list(NewLinVel)}]}],
	{Update, State1}.

%% --------------------------------------------------------------------------------------------------------------------

do_flight_control(State) ->
	Physical = State#state.physical,
	Orientation = entity_physical:get_orientation(Physical),
	PositionVelAbs = entity_physical:get_linear_velocity(Physical),
	AngularVelAbs = entity_physical:get_angular_velocity(Physical),
	OldForce = entity_physical:get_force_relative(Physical),
	OldTorque = entity_physical:get_torque_relative(Physical),

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
			NewPhysical = entity_physical:apply_update(physical, PhysicalUpdates, Physical#state.physical),

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

%% --------------------------------------------------------------------------------------------------------------------

update_vector({UpdateX, UpdateY, UpdateZ}, {CurrentX, CurrentY, CurrentZ}) ->
	{
		first_defined(UpdateX, CurrentX),
		first_defined(UpdateY, CurrentY),
		first_defined(UpdateZ, CurrentZ)
	}.

%% --------------------------------------------------------------------------------------------------------------------

first_defined(undefined, R1) ->
	R1;

first_defined(Val, _) ->
	Val.
