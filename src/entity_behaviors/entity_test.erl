%%% @doc The test entity!

-module(entity_test).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([init/2, get_client_behavior/1, get_full_state/1, client_request/5, timer_fired/2]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

init(EntityID, Behavior) ->
	InitialEntity = entity_physical:init(EntityID, Behavior),
	InitialPhysical = InitialEntity#entity.physical,
	InitialEntity#entity{
		physical = InitialPhysical#physical{
			position = {-100, 700, 10},
			orientation_vel = quaternion:from_axis_angle(
				vector:unit({
					random:uniform(),
					random:uniform(),
					random:uniform()
					}),
				random:uniform()
				)
		}
	}.

%% -------------------------------------------------------------------

get_client_behavior(Entity) ->
	{<<"Physical">>, Entity}.

%% -------------------------------------------------------------------

get_full_state(Entity) ->
	entity_physical:get_full_state(Entity).

%% -------------------------------------------------------------------

%client_request(Entity, Channel, RequestType, RequestID, Request) ->
%	ClientInfo = Entity#entity.client,
%	Connection = ClientInfo#client_info.connection,
%	Response = <<"Bumcovers.">>,
%	pre_client_connection:send(Connection, tcp, {response, RequestID}, <<"entity">>, Response),
%	{ok, Response, Entity}.

client_request(Entity, Channel, RequestType, RequestID, Request) ->
	entity_physical:client_request(Entity, Channel, RequestType, RequestID, Request).

%% -------------------------------------------------------------------

timer_fired(Entity, Tag) ->
	entity_physical:timer_fired(Entity, Tag).
