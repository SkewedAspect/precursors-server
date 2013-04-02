%%% @doc The test entity!
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_test).

-include("log.hrl").
-include("pre_entity.hrl").
-include("pre_physics.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/2, simulate/2, get_full_state/1, client_request/5, client_event/5]).

%% --------------------------------------------------------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, Behavior) ->
	InitialEntity = entity_physical:init(EntityID, Behavior),
	InitialPhysical = proplists:get_value(physical, InitialEntity#entity.state),
	Choice = random:uniform(),
	if
		Choice < 0.5 ->
			% Flying in a circle.
			InitialEntity#entity{
				state = lists:keystore(physical, 1, InitialEntity#entity.state,
					{physical, InitialPhysical#physical{
						% Updated values (assume these change every frame)
						position = {100, 500, -10},
						linear_momentum = {0, 30, 0},
						orientation = {1, 0, 0, 0},

						% Input-only values
						force_relative = {-9, 0, 0},

						% Purely calculated values (DON'T try to change these externally)
						angular_velocity = {0.9887710779360422, 0.0, 0.0, 0.14943813247359922}
					}}
				)
			};
		true ->
			% Rotating in place.
			InitialEntity#entity{
				state = lists:keystore(physical, 1, InitialEntity#entity.state,
					{physical, InitialPhysical#physical{
						position = {random:uniform() * 200 - 100, 700, 10},
						angular_velocity = quaternion:from_axis_angle(
							vector:unit({
								random:uniform(),
								random:uniform(),
								random:uniform()
							}),
							random:uniform() * math:pi()
						)
					}}
				)
			}
	end.

%% --------------------------------------------------------------------------------------------------------------------

simulate(EntityRecord, EntityEngineState) ->
	entity_physical:simulate(EntityRecord, EntityEngineState).

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(EntityState) ->
	entity_physical:get_full_state(EntityState).

%% --------------------------------------------------------------------------------------------------------------------

%client_request(EntityState, Channel, RequestType, RequestID, Request) ->
%	ClientInfo = EntityState#entity.client,
%	Connection = ClientInfo#client_info.connection,
%	Response = <<"Bumcovers.">>,
%	pre_client_connection:send(Connection, tcp, {response, RequestID}, <<"entity">>, Response),
%	{Response, EntityState}.

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
%% Internal API
%% --------------------------------------------------------------------------------------------------------------------

handle_input_command(EntityState, [{_, _} | _] = RawCommand) ->
	Command = proplists:get_value(name, RawCommand),
	Args = proplists:get_value(args, RawCommand),
	KWArgs = proplists:get_value(kwargs, RawCommand),
	handle_input_command(EntityState, Command, Args, KWArgs).

%% --------------------------------------------------------------------------------------------------------------------

handle_input_command(EntityState, <<"test">>, _Args, _KWArgs) ->
	?info("Confirming \"test\" input command."),
	Physical = proplists:get_value(physical, EntityState#entity.state),
	Response = {reply, [
		{confirm, true}
	]},
	EntityState1 = EntityState#entity{
		state = [
			{physical, Physical#physical{
				position = {random:uniform() * 200 - 100, 700, 10}
			}}
		| proplists:delete(physical, EntityState#entity.state)
		]
	},
	{Response, EntityState1};

handle_input_command(EntityState, Command, Args, KWArgs) ->
	?info("Got unrecognized input command: ~p", [{Command, Args, KWArgs}]),
	Response = {reply, [
		{confirm, false},
		{reason, <<"VALID CRAPBACK: Unrecognized input command \"", Command/binary, "\"!">>}
	]},
	{Response, EntityState}.
