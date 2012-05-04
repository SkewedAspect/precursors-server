%%% @doc The test entity!

-module(entity_ship).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([init/2, get_client_behavior/1, get_full_state/1, client_request/6, client_event/5, timer_fired/2]).

-record(ship_data, {
	target_position_vel = {0, 0, 0} :: vector:vec(),
	target_orientation_angle_vel = {0, 0, 0} :: vector:vec()
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
	entity_physical:timer_fired(EntityState, Tag).

%% -------------------------------------------------------------------

handle_input_command(EntityState, {struct, RawCommand}) ->
	Command = proplists:get_value(<<"name">>, RawCommand),
	Args = proplists:get_value(<<"args">>, RawCommand),
	KWArgs = proplists:get_value(<<"kwargs">>, RawCommand),
	handle_input_command(EntityState, Command, Args, KWArgs).

%% -------------------------------------------------------------------

% Positional target velocity
handle_input_command(EntityState, <<"sideslip">>, [TargetVel], _KWArgs) ->
	set_target_position_vel(EntityState, {TargetVel, undefined, undefined});

handle_input_command(EntityState, <<"throttle">>, [TargetVel], _KWArgs) ->
	set_target_position_vel(EntityState, {undefined, TargetVel, undefined});

handle_input_command(EntityState, <<"lift">>, [TargetVel], _KWArgs) ->
	set_target_position_vel(EntityState, {undefined, undefined, TargetVel});

% Rotational target velocity
handle_input_command(EntityState, <<"yaw">>, [TargetVel], _KWArgs) ->
	set_target_orientation_vel(EntityState, {TargetVel, undefined, undefined});

handle_input_command(EntityState, <<"pitch">>, [TargetVel], _KWArgs) ->
	set_target_orientation_vel(EntityState, {undefined, TargetVel, undefined});

handle_input_command(EntityState, <<"roll">>, [TargetVel], _KWArgs) ->
	set_target_orientation_vel(EntityState, {undefined, undefined, TargetVel});

% Catch-all
handle_input_command(EntityState, Command, Args, KWArgs) ->
	?info("Got unrecognized input command: ~p, ~p, ~p", [Command, Args, KWArgs]),
	Response = {reply, {struct, [
		{confirm, false},
		{reason, <<"VALID CRAPBACK: Unrecognized input command \"", Command/binary, "\"!">>}
	]}},
	{Response, EntityState}.

%% -------------------------------------------------------------------

set_target_orientation_vel(EntityState, {TYaw, TPitch, TRoll}) ->
	?info("Setting orientation velocity to ~p.", [{TYaw, TPitch, TRoll}]),
	#entity{
		physical = Physical,
		behavior_data = #ship_data{
			target_orientation_angle_vel = {Yaw, Pitch, Roll}
		} = ShipData
	} = EntityState,

	NewTargetVel = {
		first_defined(TYaw, Yaw),
		first_defined(TPitch, Pitch),
		first_defined(TRoll, Roll)
	},

	%XXX: HACK: For now, just set velocity directly instead of doing target velocity calculations.
	OrientationVel = quaternion:from_body_rates(vector:multiply(0.2, NewTargetVel)),

	EntityState1 = EntityState#entity{
		physical = Physical#physical{
			orientation_vel = OrientationVel
		},
		behavior_data = ShipData#ship_data{
			target_orientation_angle_vel = NewTargetVel
		}
	},
	Response = {reply, {struct, [
		{confirm, true}
	]}},
	{Response, EntityState1}.

%% -------------------------------------------------------------------

set_target_position_vel(EntityState, {TX, TY, TZ}) ->
	?info("Setting orientation velocity to ~p.", [{TX, TY, TZ}]),
	#entity{
		physical = #physical{
			orientation = Orientation
		} = Physical,
		behavior_data = #ship_data{
			target_position_vel = {X, Y, Z}
		} = ShipData
	} = EntityState,

	NewTargetVel = {
		first_defined(TX, X),
		first_defined(TY, Y),
		first_defined(TZ, Z)
	},

	%XXX: HACK: For now, just set velocity directly instead of doing target velocity calculations.
	PositionVel = quaternion:rotate(NewTargetVel, Orientation),

	EntityState1 = EntityState#entity{
		physical = Physical#physical{
			position_vel = PositionVel
		},
		behavior_data = ShipData#ship_data{
			target_position_vel = NewTargetVel
		}
	},
	Response = {reply, {struct, [
		{confirm, true}
	]}},
	{Response, EntityState1}.

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
