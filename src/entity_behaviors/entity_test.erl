%%% @doc The test entity!

-module(entity_test).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([init/2, get_client_behavior/1, get_full_state/1, client_request/6, client_event/5, timer_fired/2]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

init(EntityID, Behavior) ->
	InitialEntity = entity_physical:init(EntityID, Behavior),
	InitialPhysical = InitialEntity#entity.physical,
	Choice = random:uniform(),
	if
		Choice < 0.5 ->
			% Flying in a circle.
			InitialEntity#entity{
				physical = InitialPhysical#physical{
					position = {100, 500, -10},
					position_vel = {0, 30, 0},
					position_acc_abs = {0, 0, 0},
					position_acc_rel = {-9, 0, 0},
					orientation = {1, 0, 0, 0},
					orientation_vel = {0.9887710779360422, 0.0, 0.0, 0.14943813247359922},
					orientation_acc_abs = {1, 0, 0, 0},
					orientation_acc_rel = {1, 0, 0, 0}
				}
			};
		true ->
			% Rotating in place.
			InitialEntity#entity{
				physical = InitialPhysical#physical{
					position = {random:uniform() * 200 - 100, 700, 10},
					orientation_vel = quaternion:from_axis_angle(
						vector:unit({
							random:uniform(),
							random:uniform(),
							random:uniform()
							}),
						random:uniform() * math:pi()
						)
				}
			}
	end.

%% -------------------------------------------------------------------

get_client_behavior(EntityState) ->
	{<<"Physical">>, EntityState}.

%% -------------------------------------------------------------------

get_full_state(EntityState) ->
	entity_physical:get_full_state(EntityState).

%% -------------------------------------------------------------------

%client_request(EntityState, ClientInfo, Channel, RequestType, RequestID, Request) ->
%	ClientInfo = EntityState#entity.client,
%	Connection = ClientInfo#client_info.connection,
%	Response = <<"Bumcovers.">>,
%	pre_client_connection:send(Connection, tcp, {response, RequestID}, <<"entity">>, Response),
%	{Response, EntityState}.

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

handle_input_command(EntityState, <<"test">>, _Args, _KWArgs) ->
	?info("Confirming \"test\" input command."),
	#entity{
		physical = Physical
	} = EntityState,
	Response = {reply, {struct, [
		{confirm, true}
	]}},
	EntityState1 = EntityState#entity{
		physical = Physical#physical{
			position = {random:uniform() * 200 - 100, 700, 10}
		}
	},
	{Response, EntityState1};

handle_input_command(EntityState, Command, Args, KWArgs) ->
	?info("Got unrecognized input command: ~p", [{Command, Args, KWArgs}]),
	Response = {reply, {struct, [
		{confirm, false},
		{reason, <<"VALID CRAPBACK: Unrecognized input command \"", Command/binary, "\"!">>}
	]}},
	{Response, EntityState}.
