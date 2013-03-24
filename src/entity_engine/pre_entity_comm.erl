%%% @doc The entity/client communication interface.
%%%
%%% This module is simply an interface from the channel logic to the entity engine and vice versa.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_entity_comm).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([client_request/5, client_event/4]).
-export([broadcast_event/3, broadcast_update/2, broadcast_full_update/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------------------------------------------------------

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Passes the client request to the appropriate behavior to be handled.
%%
%% This passes a request from the client to the behavior of the entity the client is currently controlling. A response
%% is always expected.

-spec client_request(ClientInfo, Channel, RequestType, RequestID, Request) -> Response when
	ClientInfo :: #client_info{},
	Channel :: atom(),
	RequestType :: binary(),
	RequestID :: integer(),
	Request :: json(),
	Response :: json().

client_request(ClientInfo, Channel, RequestType, RequestID, Request) ->
	EntityEngine = ClientInfo#client_info.entity_engine,
	EntityID = ClientInfo#client_info.entity,				%TODO: Make this entity_id

	% Call the entity engine
	pre_entity_engine:client_request(EntityEngine, EntityID, Channel, RequestType, RequestID, Request).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Passes the client event to the appropriate behavior to be handled.
%%
%% This passes an eventvfrom the client to the behavior of the entity the client is currently controlling. No response
%% is expected.

-spec client_event(ClientInfo, Channel, EventType, Event) -> Response when
	ClientInfo :: #client_info{},
	Channel :: atom(),
	EventType :: binary(),
	Event :: json(),
	Response :: json().

client_event(ClientInfo, Channel, EventType, Event) ->
	EntityEngine = ClientInfo#client_info.entity_engine,
	EntityID = ClientInfo#client_info.entity,				%TODO: Make this entity_id

	% Call the entity engine
	pre_entity_engine:client_event(EntityEngine, EntityID, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Broadcasts an event for a given entity.

-spec broadcast_event(EventType::binary(), EntityID::binary(), EventContents::json()) -> ok.

broadcast_event(EventType, EntityID, EventContents) ->
	pre_channel_entity_sup:broadcast_event(EventType, EntityID, EventContents).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Broadcasts a delta update for a given entity.

-spec broadcast_update(EntityID::binary(), PartialState::json()) -> ok.

broadcast_update(EntityID, PartialState) ->
	pre_channel_entity_sup:broadcast_update(EntityID, PartialState).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Broadcasts a full update for a given entity.

-spec broadcast_full_update(Entity::#entity{}) -> ok.

broadcast_full_update(Entity) ->
	pre_channel_entity_sup:broadcast_event(Entity).
