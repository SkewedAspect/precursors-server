%%% @doc An entity representing a ship.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_ship).

-include_lib("pre_channel/include/pre_entity.hrl").

-behaviour(entity_controller).

% pre_entity
-export([init/1, simulate/2, get_client_controller/0, get_full_state/1, client_request/5, client_event/4,
	entity_event/3, apply_update/3]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(InitialEntity) ->
	% Defaults for physical
	Entity = case dict:find(physical, InitialEntity#entity.state) of
		{ok, _} ->
			entity_physical:init(InitialEntity);
		error ->
			% The initial entity had no 'physical' key; set up a default with randomized values.
			EntWithNoInitialPhysical = entity_physical:init(InitialEntity),
			InitialState = EntWithNoInitialPhysical#entity.state,
			InitialPhysical = dict:fetch(physical, InitialState),

			% Set up default physical state with random position/orientation
			DefaultPhysical = pre_physics_rk4:update_from_proplist(
				InitialPhysical,
				[
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
					}
				]
			),

			EntWithNoInitialPhysical#entity{
				state = dict:store(physical, DefaultPhysical, InitialState)
			}
	end,

	% -------------------------------------------------------------------------

	% Defaults for modelDef
	State1 = case dict:find(modelDef, Entity#entity.state) of
		{ok, _} ->
			Entity#entity.state;
		error ->
			dict:store(modelDef, [{model, <<"Ships/ares">>}], Entity#entity.state)
	end,

	% -------------------------------------------------------------------------

	% Set up ship state
	InitialShip = case dict:find(ship, State1) of
		{ok, Value} ->
			Value;
		error ->
			dict:new()
	end,

	DefaultShip = dict:from_list([
		{ target_linear_velocity, {0, 0, 0} },
		{ target_angular_velocity, {0, 0, 0} },

		% Intrinsic ship parameters
		{ linear_target_velocity_scaling, {1200, 1000, 1600} }, % {sideslip, lift, throttle}
		{ angular_target_velocity_scaling, {2, 2, 2} }, %  {pitch, heading, roll}
		{ max_linear_thrust, {600, 500, 800} }, % {sideslip, lift, throttle}
		{ max_angular_thrust, {2, 2, 2} }, %  {pitch, heading, roll}
		{ linear_responsiveness, {3, 3, 3} }, % {sideslip, lift, throttle}
		{ angular_responsiveness, {3, 3, 3} } %  {pitch, heading, roll}
	]),

	% Merge our initial shipstate dict with our default values, prefering our initials where there are
	% conflicts.
	Ship = dict:merge(fun(_Key, InitialVal, _DefaultVal) ->
		InitialVal
	end, InitialShip, DefaultShip),

	% -------------------------------------------------------------------------

	% Set up our initial state
	State2 = dict:store(ship, Ship, State1),

	Entity#entity{
		state = State2
	}.

%% --------------------------------------------------------------------------------------------------------------------

get_client_controller() ->
	<<"Ship">>.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(Entity) ->
	ShipFullState = dict:fetch(ship, Entity#entity.state),
	PhysicalFullState = entity_physical:get_full_state(Entity),

	[
		{ship, entity_base:gen_full_state(
			fun (Value) ->
				case Value of
					{_, _, _} ->
						vector:vec_to_list(Value);
					{_, _, _, _} ->
						quaternion:quat_to_list(Value);
					_ ->
						Value
				end
			end,
			ShipFullState
		)}
		| PhysicalFullState
	].

%% --------------------------------------------------------------------------------------------------------------------

client_request(Entity, input, <<"command">>, _RequestID, Request) ->
	handle_input_command(Entity, Request);

client_request(Entity, Channel, RequestType, RequestID, Request) ->
	entity_physical:client_request(Entity, Channel, RequestType, RequestID, Request).

%% --------------------------------------------------------------------------------------------------------------------

client_event(Entity, input, <<"command">>, Event) ->
	{_Response, Update, Entity1} = handle_input_command(Entity, Event),
	{Update, Entity1};

client_event(Entity, Channel, EventType, Event) ->
	entity_physical:client_event(Entity, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------

entity_event(Event, From, Entity) ->
	entity_physical:client_event(Event, From, Entity).

%% --------------------------------------------------------------------------------------------------------------------

apply_update(ship, ShipUpdate, Entity) ->
	ShipState = dict:fetch(ship, Entity#entity.state),

	NewShip = lists:foldl(
		fun
			({Key, [_, _, _] = NewValue}, ShipState1) ->
				dict:store(Key, vector:to_vec(NewValue), ShipState1);
			({Key, [_, _, _, _] = NewValue}, ShipState1) ->
				dict:store(Key, vector:to_vec(NewValue), ShipState1);
			({Key, NewValue}, ShipState1) ->
				dict:store(Key, NewValue, ShipState1)
		end,
		ShipState,
		ShipUpdate
	),

	Entity1 = Entity#entity {
		state = dict:store(ship, NewShip, Entity#entity.state)
	},
	{undefined, Entity1};

apply_update(Key, Value, Entity) ->
	entity_physical:apply_update(Key, Value, Entity).

%% --------------------------------------------------------------------------------------------------------------------

simulate(Entity, EntityEngineState) ->
	{ShipUpdate, Entity1} = do_flight_control(Entity),
	PhysicalResult = entity_physical:simulate(Entity1, EntityEngineState),

	case PhysicalResult of
		{undefined, Entity2} ->
			{ShipUpdate, Entity2};
		{PhysicalUpdate, Entity3} ->
			case ShipUpdate of
				undefined ->
					{PhysicalUpdate, Entity3};
				_ ->
					{ShipUpdate ++ PhysicalUpdate, Entity3}
			end
	end.

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

handle_input_command(Entity, <<"lift">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(Entity, {undefined, TargetVel, undefined});

handle_input_command(Entity, <<"throttle">>, [TargetVel], _KWArgs) ->
	set_target_linear_velocity(Entity, {undefined, undefined, -TargetVel});

% Rotational target velocity
handle_input_command(Entity, <<"pitch">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(Entity, {TargetVel, undefined, undefined});

handle_input_command(Entity, <<"heading">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(Entity, {undefined, TargetVel, undefined});

handle_input_command(Entity, <<"roll">>, [TargetVel], _KWArgs) ->
	set_target_angular_velocity(Entity, {undefined, undefined, TargetVel});

% Catch-all
handle_input_command(Entity, Command, Args, KWArgs) ->
	lager:info("Got unrecognized input command: ~p, ~p, ~p", [Command, Args, KWArgs]),
	Response = [
		{confirm, false},
		{reason, <<"Unrecognized input command \"", Command/binary, "\"!">>}
	],
	{Response, undefined, Entity}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_angular_velocity(Entity, RequestedAngVel) ->
	ShipState = dict:fetch(ship, Entity#entity.state),
	CurrentAngVel = dict:fetch(target_angular_velocity, ShipState),
	set_target_angular_velocity(Entity, ShipState, CurrentAngVel, update_vector(RequestedAngVel, CurrentAngVel)).

set_target_angular_velocity(Entity, _ShipState, CurAndNewAngVel, CurAndNewAngVel) ->
	% No change; confirm, but don't produce an update.
	Response = [{confirm, true}],
	{Response, undefined, Entity};

set_target_angular_velocity(Entity, ShipState, _CurrentAngVel, NewAngVel) ->
	%?debug("Setting orientation velocity to ~p.", [NewAngVel]),
	% Update the target angular velocity
	NewShipState = dict:store(target_angular_velocity, NewAngVel, ShipState),

	% Update State
	Entity1 = Entity#entity{
		state = dict:store(ship, NewShipState, Entity#entity.state)
	},

	% Build response and update
	Response = [{confirm, true}],
	Update = [{ship, [{target_angular_velocity, vector:vec_to_list(NewAngVel)}]}],

	{Response, Update, Entity1}.

%% --------------------------------------------------------------------------------------------------------------------

set_target_linear_velocity(Entity, RequestedLinVel) ->
	ShipState = dict:fetch(ship, Entity#entity.state),
	CurrentLinVel = dict:fetch(target_linear_velocity, ShipState),
	set_target_linear_velocity(Entity, ShipState, CurrentLinVel, update_vector(RequestedLinVel, CurrentLinVel)).

set_target_linear_velocity(Entity, _ShipState, CurAndNewLinVel, CurAndNewLinVel) ->
	% No change; confirm, but don't produce an update.
	Response = [{confirm, true}],
	{Response, undefined, Entity};

set_target_linear_velocity(Entity, ShipState, _CurrentLinVel, NewLinVel) ->
	%?debug("Setting position velocity to ~p.", [NewLinVel]),
	% Update the target linear velocity
	NewShipState = dict:store(target_linear_velocity, NewLinVel, ShipState),

	% Update State
	Entity1 = Entity#entity{
		state = dict:store(ship, NewShipState, Entity#entity.state)
	},

	% Build response and update
	Response = [{confirm, true}],
	Update = [{ship, [{target_linear_velocity, vector:vec_to_list(NewLinVel)}]}],

	{Response, Update, Entity1}.

%% --------------------------------------------------------------------------------------------------------------------

do_flight_control(Entity) ->
	Physical = dict:fetch(physical, Entity#entity.state),
	Orientation = pre_physics_rk4:get_prop(orientation, Physical),
	PositionVelAbs = pre_physics_rk4:get_prop(linear_velocity, Physical),
	AngularVelAbs = pre_physics_rk4:get_prop(angular_velocity, Physical),
	OldForce = pre_physics_rk4:get_prop(force_relative, Physical),
	OldTorque = pre_physics_rk4:get_prop(torque_relative, Physical),

	ShipState = dict:fetch(ship, Entity#entity.state),
	{TX, TY, TZ} = dict:fetch(target_linear_velocity, ShipState),
	{TPitch, THeading, TRoll} = dict:fetch(target_angular_velocity, ShipState),
	{TXScale, TYScale, TZScale} = dict:fetch(linear_target_velocity_scaling, ShipState),
	{TPitchScale, THeadingScale, TRollScale} = dict:fetch(angular_target_velocity_scaling, ShipState),
	{MaxXT, MaxYT, MaxZT} = dict:fetch(max_linear_thrust, ShipState),
	{MaxPitchT, MaxHeadingT, MaxRollT} = dict:fetch(max_angular_thrust, ShipState),
	{XR, YR, ZR} = dict:fetch(linear_responsiveness, ShipState),
	{PitchR, HeadingR, RollR} = dict:fetch(angular_responsiveness, ShipState),

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
			{undefined, Entity};
		_ ->
			% Update physical state
			NewPhysical = pre_physics_rk4:update_from_proplist(Physical, PhysicalUpdates),

			% Update entity state
			Entity1 = Entity#entity{
				state = dict:store(physical, NewPhysical, Entity#entity.state)
			},

			% Create delta update
			Update = [{physical, [{K, vector:vec_to_list(V)} || {K, V} <- PhysicalUpdates]}],

			{Update, Entity1}
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
