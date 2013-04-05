%%% @doc An entity representing a ship.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_ship).

-include("log.hrl").
-include("pre_entity.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/1, simulate/2, get_full_state/1, client_request/5, client_event/5]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(Entity) ->
	InitialEntity = entity_physical:init(Entity),

	% -------------------------------------------------------------------------

	% Set up initial physical state
	InitialPhysical = dict:fetch(physical, InitialEntity#entity.state),

	% Set up default physical state with random position/orientation
	DefaultPhysical = dict:from_list([
		{
			position, {
				random:uniform() * 200 - 100,
				random:uniform() * 200 + 600,
				random:uniform() * 20
			}
		},
		{
			orientation, quaternion:from_axis_angle(
				vector:unit({
					random:uniform(),
					random:uniform(),
					random:uniform()
				}),
				random:uniform() * math:pi()
			)
		}
	]),

	% Merge our initial physical state dict with our default values, prefering our initials where there's
	% conflicts.
	Physical = dict:merge(fun(_Key, InitialVal, _DefaultVal) ->
		InitialVal
	end, InitialPhysical, DefaultPhysical),

	% -------------------------------------------------------------------------

	% Set up ship state
	InitialShip = case dict:find(ship, InitialEntity#entity.state) of
		{ok, Value} ->
			Value;
		error ->
			dict:new()
	end,

	DefaultShip = dict:from_list([
		{ target_linear_velocity, {0, 0, 0} },
		{ target_angular_velocity, {0, 0, 0} },

		% Intrinsic ship parameters
		{ linear_target_velocity_scaling, {600, 800, 500} }, % {sideslip, throttle, lift}
		{ angular_target_velocity_scaling, {2, 2, 2} }, %  {pitch, roll, yaw}
		{ max_linear_thrust, {300, 400, 250} }, % {sideslip, throttle, lift}
		{ max_angular_thrust, {2, 2, 2} }, %  {pitch, roll, yaw}
		{ linear_responsiveness, {3, 3, 3} }, % {sideslip, throttle, lift}
		{ angular_responsiveness, {3, 3, 3} } %  {pitch, roll, yaw}
	]),

	% Merge our initial shipstate dict with our default values, prefering our initials where there's
	% conflicts.
	Ship = dict:merge(fun(_Key, InitialVal, _DefaultVal) ->
		InitialVal
	end, InitialShip, DefaultShip),

	% -------------------------------------------------------------------------

	% Set up our initial state
	InitialState = InitialEntity#entity.state,
	State1 = dict:store(physical, Physical, InitialState),
	State2 = dict:store(ship, Ship, State1),

	InitialEntity#entity{
		state = State2
	}.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(Entity) ->
	entity_physical:get_full_state(Entity).

%% --------------------------------------------------------------------------------------------------------------------

client_request(Entity, input, <<"command">>, _RequestID, Request) ->
	handle_input_command(Entity, Request);

client_request(Entity, Channel, RequestType, RequestID, Request) ->
	entity_physical:client_request(Entity, Channel, RequestType, RequestID, Request).

%% --------------------------------------------------------------------------------------------------------------------

client_event(Entity, _ClientInfo, input, <<"command">>, Event) ->
	{_Response, Entity1} = handle_input_command(Entity, Event),
	{noreply, Entity1};

client_event(Entity, ClientInfo, Channel, EventType, Event) ->
	entity_physical:client_event(Entity, ClientInfo, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------

simulate(Entity, EntityEngineState) ->
	Entity1 = do_flight_control(Entity),
	entity_physical:simulate(Entity1, EntityEngineState).

%% --------------------------------------------------------------------------------------------------------------------

handle_input_command(Entity, [{_, _} | _] = RawCommand) ->
	Command = proplists:get_value(name, RawCommand),
	Args = proplists:get_value(args, RawCommand),
	KWArgs = proplists:get_value(kwargs, RawCommand),
	handle_input_command(Entity, Command, Args, KWArgs).

%% --------------------------------------------------------------------------------------------------------------------

% Positional target velocity
handle_input_command(Entity, <<"sideslip">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(Entity, {TargetVel, undefined, undefined});

handle_input_command(Entity, <<"throttle">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(Entity, {undefined, TargetVel, undefined});

handle_input_command(Entity, <<"lift">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(Entity, {undefined, undefined, TargetVel});

% Rotational target velocity
handle_input_command(Entity, <<"pitch">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(Entity, {TargetVel, undefined, undefined});

handle_input_command(Entity, <<"roll">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(Entity, {undefined, TargetVel, undefined});

handle_input_command(Entity, <<"yaw">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(Entity, {undefined, undefined, TargetVel});

% Catch-all
handle_input_command(Entity, Command, Args, KWArgs) ->
	?info("Got unrecognized input command: ~p, ~p, ~p", [Command, Args, KWArgs]),
	Response = {reply, [
		{confirm, false},
		{reason, <<"Unrecognized input command \"", Command/binary, "\"!">>}
	]},
	{Response, Entity}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_angular_velocity(Entity, NewAngVel) ->
	%?debug("Setting orientation velocity to ~p.", [NewAngVel]),
	ShipState = dict:fetch(ship, Entity#entity.state),
	CurrentAngVel = dict:fetch(target_angular_velocity, ShipState),

	% Update the target angular velocity
	NewShipState = dict:store(target_angular_velocity, update_vector(NewAngVel, CurrentAngVel), ShipState),

	% Update State
	Entity1 = Entity#entity{
		state = dict:store(ship, NewShipState, Entity#entity.state)
	},

	% Build response
	Response = {reply, [{confirm, true}]},

	{Response, Entity1}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_linear_velocity(Entity, NewLinVel) ->
	%?debug("Setting position velocity to ~p.", [NewLinVel]),
	ShipState = dict:fetch(ship, Entity#entity.state),
	CurrentLinVel = dict:fetch(target_linear_velocity, ShipState),

	% Update the target linear velocity
	NewShipState = dict:store(target_linear_velocity, update_vector(NewLinVel, CurrentLinVel), ShipState),

	% Update State
	Entity1 = Entity#entity{
		state = dict:store(ship, NewShipState, Entity#entity.state)
	},

	% Build response
	Response = {reply, [{confirm, true}]},

	{Response, Entity1}.

%% --------------------------------------------------------------------------------------------------------------------

do_flight_control(Entity) ->
	Physical = dict:fetch(physical, Entity#entity.state),
	Orientation = dict:fetch(orientation, Physical),
	PositionVelAbs = dict:fetch(linear_velocity, Physical),
	AngularVelAbs = dict:fetch(angular_velocity, Physical),

	ShipState = dict:fetch(ship, Entity#entity.state),
	{TX, TY, TZ} = dict:fetch(target_linear_velocity, ShipState),
	{TPitch, TRoll, TYaw} = dict:fetch(target_angular_velocity, ShipState),
	{TXScale, TYScale, TZScale} = dict:fetch(linear_target_velocity_scaling, ShipState),
	{TPitchScale, TRollScale, TYawScale} = dict:fetch(angular_target_velocity_scaling, ShipState),
	{MaxXT, MaxYT, MaxZT} = dict:fetch(max_linear_thrust, ShipState),
	{MaxPitchT, MaxRollT, MaxYawT} = dict:fetch(max_angular_thrust, ShipState),
	{XR, YR, ZR} = dict:fetch(linear_responsiveness, ShipState),
	{PitchR, RollR, YawR} = dict:fetch(angular_responsiveness, ShipState),

	% Get the ship-relative linear velocity
	{XVel, YVel, ZVel} = quaternion:rotate(PositionVelAbs, quaternion:reciprocal(Orientation)),

	% Get the ship-relative angular velocity
	{PitchVel, RollVel, YawVel} = quaternion:rotate(AngularVelAbs, quaternion:reciprocal(Orientation)),

	% Calculate relative force
	Force = {
		calc_thrust(MaxXT, XR, XVel, TXScale * TX),
		calc_thrust(MaxYT, YR, YVel, TYScale * TY),
		calc_thrust(MaxZT, ZR, ZVel, TZScale * TZ)
	},

	% Calculate relative torque
	Torque = {
		calc_thrust(MaxPitchT, PitchR, PitchVel, TPitchScale * TPitch),
		calc_thrust(MaxRollT, RollR, RollVel, TRollScale * TRoll),
		calc_thrust(MaxYawT, YawR, YawVel, TYawScale * TYaw)
	},

	% Update physical state
	NewPhysical1 = dict:store(force_relative, Force, Physical),
	NewPhysical2 = dict:store(torque_relatice, Torque, NewPhysical1),

	% Update entity state
	Entity#entity{
		state = dict:store(physical, NewPhysical2, Entity#entity.state)
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
