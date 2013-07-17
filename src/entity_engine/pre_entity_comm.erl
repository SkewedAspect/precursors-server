%%% @doc The entity/client communication interface.
%%%
%%% This module is simply an interface from the channel logic to the entity engine and vice versa.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_entity_comm).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([client_request/5, client_request/6, client_event/4, client_event/5]).
-export([send_update/3]).

%% --------------------------------------------------------------------------------------------------------------------
%% External API
%% --------------------------------------------------------------------------------------------------------------------

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Passes the client request to the appropriate behavior to be handled.
%%
%% This passes a request from the client to the behavior of the entity the client is currently controlling. A response
%% is always expected.

-spec client_request(ClientInfo | EntityID, Channel, RequestType, RequestID, Request) -> Response when
	ClientInfo :: #client_info{},
	EntityID :: binary(),
	Channel :: atom(),
	RequestType :: binary(),
	RequestID :: integer(),
	Request :: json(),
	Response :: json().

client_request(#client_info{} = ClientInfo, Channel, RequestType, RequestID, Request) ->
	#client_info{
		entity_engine = EntityEngine,
		entity = EntityID				%TODO: Make this 'entity_id' instead of 'entity'
	} = ClientInfo,

	% Call the entity engine
	client_request(EntityEngine, EntityID, Channel, RequestType, RequestID, Request);

client_request(EntityID, Channel, RequestType, RequestID, Request) ->
	EntityEngine = pre_entity_engine_sup:get_entity_engine(EntityID),

	% Call the entity engine
	client_request(EntityEngine, EntityID, Channel, RequestType, RequestID, Request).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Passes the client request to the appropriate behavior to be handled.
%%
%% This passes a request from the client to the behavior of the entity the client is currently controlling. A response
%% is always expected.

-spec client_request(EntityEngine, EntityID, Channel, RequestType, RequestID, Request) -> Response when
	EntityEngine :: pid(),
	EntityID :: binary(),
	Channel :: atom(),
	RequestType :: binary(),
	RequestID :: integer(),
	Request :: json(),
	Response :: json().

client_request(EntityEngine, EntityID, Channel, RequestType, RequestID, Request) ->
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
	#client_info {
		entity_engine = EntityEngine,
		entity = EntityID				%TODO: Make this 'entity_id' instead of 'entity'
	} = ClientInfo,

	% Call the entity engine
	client_event(EntityEngine, EntityID, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Passes the client event to the appropriate behavior to be handled.
%%
%% This passes an eventvfrom the client to the behavior of the entity the client is currently controlling. No response
%% is expected.

-spec client_event(EntityEngine, EntityID, Channel, EventType, Event) -> Response when
	EntityEngine :: pid(),
	EntityID :: binary(),
	Channel :: atom(),
	EventType :: binary(),
	Event :: json(),
	Response :: json().

client_event(EntityEngine, EntityID, Channel, EventType, Event) ->
	% Call the entity engine
	pre_entity_engine:client_event(EntityEngine, EntityID, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Sends a delta update to the given client.

-spec send_update(ClientInfo :: #client_info{}, EntityID :: binary(), Update :: json()) -> ok.

send_update(ClientInfo, EntityID, Update) ->
	ConnectionPid = ClientInfo#client_info.connection,

	Update1 = pre_channel_entity:build_state_event(update, Update, EntityID),
	pre_client_connection:send(ConnectionPid, udp, event, entity, Update1).
