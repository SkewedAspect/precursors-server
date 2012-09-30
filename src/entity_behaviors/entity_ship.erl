%%% @doc The test entity!

-module(entity_ship).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([init/2, get_client_behavior/1, get_full_state/1, client_request/6, client_event/5, timer_fired/2]).

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

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

init(EntityID, Behavior) ->
	InitialEntity = entity_physical:init(EntityID, Behavior),
	InitialPhysical = InitialEntity#entity.physical,
	% Random initial position and orientation.
	InitialEntity#entity{
		physical = InitialPhysical#physical{
			position = {random:uniform() * 200 - 100, random:uniform() * 200 + 600, random:uniform() * 20},
			orientation = quaternion:from_axis_angle(
				vector:unit({
					random:uniform(),
					random:uniform(),
					random:uniform()
					}),
				random:uniform() * math:pi()
				)
		},
		behavior_data = #ship_data{}
	}.

%% -------------------------------------------------------------------

get_client_behavior(EntityState) ->
	{<<"Physical">>, EntityState}.

%% -------------------------------------------------------------------

get_full_state(EntityState) ->
	entity_physical:get_full_state(EntityState).

%% -------------------------------------------------------------------

client_request(EntityState, _ClientInfo, input, <<"command">>, _RequestID, Request) ->
	handle_input_command(EntityState, Request);

client_request(EntityState, ClientInfo, Channel, RequestType, RequestID, Request) ->
	entity_physical:client_request(EntityState, ClientInfo, Channel, RequestType, RequestID, Request).

%% -------------------------------------------------------------------

client_event(EntityState, _ClientInfo, input, <<"command">>, Event) ->
	{_Response, EntityState1} = handle_input_command(EntityState, Event),
	{noreply, EntityState1};

client_event(EntityState, ClientInfo, Channel, EventType, Event) ->
	entity_physical:client_event(EntityState, ClientInfo, Channel, EventType, Event).

%% -------------------------------------------------------------------

timer_fired(EntityState, Tag) ->
	EntityState1 = do_flight_control(EntityState),
	entity_physical:timer_fired(EntityState1, Tag).

%% -------------------------------------------------------------------

handle_input_command(EntityState, {struct, RawCommand}) ->
	Command = proplists:get_value(<<"name">>, RawCommand),
	Args = proplists:get_value(<<"args">>, RawCommand),
	KWArgs = proplists:get_value(<<"kwargs">>, RawCommand),
	handle_input_command(EntityState, Command, Args, KWArgs).

%% -------------------------------------------------------------------

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
	Response = {reply, {struct, [
		{confirm, false},
		{reason, <<"VALID CRAPBACK: Unrecognized input command \"", Command/binary, "\"!">>}
	]}},
	{Response, EntityState}.

%% -------------------------------------------------------------------

set_target_angular_velocity(EntityState, {TPitch, TRoll, TYaw}) ->
	?info("Setting orientation velocity to ~p.", [{TPitch, TRoll, TYaw}]),
	#entity{
		behavior_data = #ship_data{
			target_angular_velocity = {Pitch, Roll, Yaw}
		} = ShipData
	} = EntityState,

	NewTPitch = first_defined(TPitch, Pitch),
	NewTRoll = first_defined(TRoll, Roll),
	NewTYaw = first_defined(TYaw, Yaw),

	EntityState1 = EntityState#entity{
		behavior_data = ShipData#ship_data{
			target_angular_velocity = {NewTPitch, NewTRoll, NewTYaw}
		}
	},
	Response = {reply, {struct, [
		{confirm, true}
	]}},
	{Response, EntityState1}.

%% -------------------------------------------------------------------

set_target_linear_velocity(EntityState, {TX, TY, TZ}) ->
	?info("Setting orientation velocity to ~p.", [{TX, TY, TZ}]),
	#entity{
		behavior_data = #ship_data{
			target_linear_velocity = {X, Y, Z}
		} = ShipData
	} = EntityState,

	NewTX = first_defined(TX, X),
	NewTY = first_defined(TY, Y),
	NewTZ = first_defined(TZ, Z),

	EntityState1 = EntityState#entity{
		behavior_data = ShipData#ship_data{
			target_linear_velocity = {NewTX, NewTY, NewTZ}
		}
	},
	Response = {reply, {struct, [
		{confirm, true}
	]}},
	{Response, EntityState1}.

%% -------------------------------------------------------------------

do_flight_control(EntityState) ->
	#entity{
		physical = #physical{
			orientation = Orientation,
			linear_velocity = PositionVelAbs,
			% Since Euler vectors are {X, Y, Z} where unit({X, Y, Z}) is the axis of rotation and magnitude({X, Y, Z})
			% is the speed of rotation, that works out to {1, 0, 0} being pitch (rotation around X), {0, 1, 0} being
			% roll (rotation around Y), etc. Therefore, decomposing each of the rotations gives us {Pitch, Roll, Yaw}.
			angular_velocity = AngularVelAbs
		} = Physical,
		behavior_data = #ship_data{
			target_linear_velocity = {TX, TY, TZ},
			target_angular_velocity = {TPitch, TRoll, TYaw},
			linear_target_velocity_scaling = {TXScale, TYScale, TZScale},
			angular_target_velocity_scaling = {TPitchScale, TRollScale, TYawScale},
			max_linear_thrust = {MaxXT, MaxYT, MaxZT},
			max_angular_thrust = {MaxPitchT, MaxRollT, MaxYawT},
			linear_responsiveness = {XR, YR, ZR},
			angular_responsiveness = {PitchR, RollR, YawR}
		}
	} = EntityState,

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
		physical = Physical#physical{
			force_relative = Force,
			torque_relative = Torque
		}
	}.

calc_thrust(MaxTh, Resp, CurVel, TargetVel) ->
	DMToP = 2 * MaxTh / math:pi(),
	DMToP * math:atan((TargetVel - CurVel) * Resp / DMToP).

%% -------------------------------------------------------------------

first_defined(undefined, R1) ->
	R1;

first_defined(Val, _) ->
	Val.

%% -------------------------------------------------------------------

first_defined(undefined, R1, R2) ->
	first_defined(R1, R2);

first_defined(Val, _, _) ->
	Val.

%% -------------------------------------------------------------------

first_defined(undefined, R1, R2, R3) ->
	first_defined(R1, R2, R3);

first_defined(Val, _, _, _) ->
	Val.