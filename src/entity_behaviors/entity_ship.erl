%%% @doc An entity representing a ship.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_ship).

-include("log.hrl").
-include("pre_entity.hrl").
-include("pre_physics.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/2, simulate/2, get_full_state/1, client_request/5, client_event/5]).

-record(ship_data, {
	% Current state
	target_linear_velocity = {0, 0, 0} :: vector:vec(),
	target_angular_velocity = {0, 0, 0} :: vector:vec(),

	% Intrinsic ship parameters
	linear_target_velocity_scaling = {600, 800, 500} :: vector:vec(), % {sideslip, throttle, lift}
	angular_target_velocity_scaling = {2, 2, 2} :: vector:vec(), %  {pitch, roll, yaw}
	max_linear_thrust = {300, 400, 250} :: vector:vec(), % {sideslip, throttle, lift}
	max_angular_thrust = {2, 2, 2} :: vector:vec(), %  {pitch, roll, yaw}
	linear_responsiveness = {3, 3, 3} :: vector:vec(), % {sideslip, throttle, lift}
	angular_responsiveness = {3, 3, 3} :: vector:vec() %  {pitch, roll, yaw}
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, Behavior) ->
	InitialEntity = entity_physical:init(EntityID, Behavior),
	InitialPhysical = proplists:get_value(physical, InitialEntity#entity.state),
	% Random initial position and orientation.
	InitialEntity#entity{
		state = lists:keystore(physical, 1,
			lists:keystore(ship, 1, InitialEntity#entity.state, {ship, #ship_data{}}),
			{physical, InitialPhysical#physical{
				position = {random:uniform() * 200 - 100, random:uniform() * 200 + 600, random:uniform() * 20},
				orientation = quaternion:from_axis_angle(
					vector:unit({
						random:uniform(),
						random:uniform(),
						random:uniform()
					}),
					random:uniform() * math:pi()
				)
			}}
		)
	}.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(EntityState) ->
	entity_physical:get_full_state(EntityState).

%% --------------------------------------------------------------------------------------------------------------------

client_request(EntityState, input, <<"command">>, _RequestID, Request) ->
	handle_input_command(EntityState, Request);

client_request(EntityState, Channel, RequestType, RequestID, Request) ->
	entity_physical:client_request(EntityState, Channel, RequestType, RequestID, Request).

%% --------------------------------------------------------------------------------------------------------------------

client_event(EntityState, _ClientInfo, input, <<"command">>, Event) ->
	{_Response, EntityState1} = handle_input_command(EntityState, Event),
	{noreply, EntityState1};

client_event(EntityState, ClientInfo, Channel, EventType, Event) ->
	entity_physical:client_event(EntityState, ClientInfo, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------

simulate(EntityState, EntityEngineState) ->
	EntityState1 = do_flight_control(EntityState),
	entity_physical:simulate(EntityState1, EntityEngineState).

%% --------------------------------------------------------------------------------------------------------------------

handle_input_command(EntityState, [{_, _} | _] = RawCommand) ->
	Command = proplists:get_value(name, RawCommand),
	Args = proplists:get_value(args, RawCommand),
	KWArgs = proplists:get_value(kwargs, RawCommand),
	handle_input_command(EntityState, Command, Args, KWArgs).

%% --------------------------------------------------------------------------------------------------------------------

% Positional target velocity
handle_input_command(EntityState, <<"sideslip">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(EntityState, {TargetVel, undefined, undefined});

handle_input_command(EntityState, <<"throttle">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(EntityState, {undefined, TargetVel, undefined});

handle_input_command(EntityState, <<"lift">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(EntityState, {undefined, undefined, TargetVel});

% Rotational target velocity
handle_input_command(EntityState, <<"pitch">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(EntityState, {TargetVel, undefined, undefined});

handle_input_command(EntityState, <<"roll">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(EntityState, {undefined, TargetVel, undefined});

handle_input_command(EntityState, <<"yaw">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(EntityState, {undefined, undefined, TargetVel});

% Catch-all
handle_input_command(EntityState, Command, Args, KWArgs) ->
	?info("Got unrecognized input command: ~p, ~p, ~p", [Command, Args, KWArgs]),
	Response = {reply, [
		{confirm, false},
		{reason, <<"VALID CRAPBACK: Unrecognized input command \"", Command/binary, "\"!">>}
	]},
	{Response, EntityState}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_angular_velocity(EntityState, New) ->
	%?debug("Setting orientation velocity to ~p.", [New]),
	ShipData = proplists:get_value(ship, EntityState#entity.state),
	#ship_data{
		target_angular_velocity = Current
	} = ShipData,

	EntityState1 = EntityState#entity{
		state = lists:keystore(ship, 1, EntityState#entity.state, {ship,
			ShipData#ship_data{
				target_angular_velocity = update_vector(New, Current)
			}
		})
	},
	Response = {reply, [
		{confirm, true}
	]},
	{Response, EntityState1}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_linear_velocity(EntityState, New) ->
	%?debug("Setting position velocity to ~p.", [New]),
	ShipData = proplists:get_value(ship, EntityState#entity.state),
	#ship_data{
		target_angular_velocity = Current
	} = ShipData,

	EntityState1 = EntityState#entity{
		state = lists:keystore(ship, 1, EntityState#entity.state, {ship,
			ShipData#ship_data{
				target_linear_velocity = update_vector(New, Current)
			}
		})
	},
	Response = {reply, [
		{confirm, true}
	]},
	{Response, EntityState1}.

%% --------------------------------------------------------------------------------------------------------------------

do_flight_control(EntityState) ->
	Physical = proplists:get_value(physical, EntityState#entity.state),
	#physical{
		orientation = Orientation,
		linear_velocity = PositionVelAbs,
		% Since Euler vectors are {X, Y, Z} where unit({X, Y, Z}) is the axis of rotation and magnitude({X, Y, Z})
		% is the speed of rotation, that works out to {1, 0, 0} being pitch (rotation around X), {0, 1, 0} being
		% roll (rotation around Y), etc. Therefore, decomposing each of the rotations gives us {Pitch, Roll, Yaw}.
		angular_velocity = AngularVelAbs
	} = Physical,

	ShipData = proplists:get_value(ship, EntityState#entity.state),
	#ship_data{
		target_linear_velocity = {TX, TY, TZ},
		target_angular_velocity = {TPitch, TRoll, TYaw},
		linear_target_velocity_scaling = {TXScale, TYScale, TZScale},
		angular_target_velocity_scaling = {TPitchScale, TRollScale, TYawScale},
		max_linear_thrust = {MaxXT, MaxYT, MaxZT},
		max_angular_thrust = {MaxPitchT, MaxRollT, MaxYawT},
		linear_responsiveness = {XR, YR, ZR},
		angular_responsiveness = {PitchR, RollR, YawR}
	} = ShipData,

	{XVel, YVel, ZVel} = quaternion:rotate(PositionVelAbs, quaternion:reciprocal(Orientation)),
	{PitchVel, RollVel, YawVel} = quaternion:rotate(AngularVelAbs, quaternion:reciprocal(Orientation)),

	Force = {
		calc_thrust(MaxXT, XR, XVel, TXScale * TX),
		calc_thrust(MaxYT, YR, YVel, TYScale * TY),
		calc_thrust(MaxZT, ZR, ZVel, TZScale * TZ)
	},

	Torque = {
		calc_thrust(MaxPitchT, PitchR, PitchVel, TPitchScale * TPitch),
		calc_thrust(MaxRollT, RollR, RollVel, TRollScale * TRoll),
		calc_thrust(MaxYawT, YawR, YawVel, TYawScale * TYaw)
	},

	EntityState#entity{
		state = lists:keystore(physical, 1, EntityState#entity.state,
			{physical, Physical#physical{
				force_relative = Force,
				torque_relative = Torque
			}}
		)
	}.

calc_thrust(MaxTh, Resp, CurVel, TargetVel) ->
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
