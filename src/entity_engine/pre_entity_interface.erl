%%% @doc The entity/client interface.
%%%
%%% This module is simply an interface from the channel logic to the entity engine and vice versa.
%%%
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_entity_interface).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([client_request/5, client_event/4]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Passes the client request to the appropriate behavior to be handled.
%%
%% This passes a request from the client to the behavior of the entity the client is currently controlling. A response
%% is always expected.
-spec client_request(ClientInfo::#client_info{}, Channel::atom(), RequestType::binary(), RequestID::integer(), Request::json()) ->
	Response::json().

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
-spec client_event(ClientInfo::#client_info{}, Channel::atom(), EventType::binary(), Event::json()) ->
	Response::json().

client_event(ClientInfo, Channel, EventType, Event) ->
	EntityEngine = ClientInfo#client_info.entity_engine,
	EntityID = ClientInfo#client_info.entity,				%TODO: Make this entity_id

	% Call the entity engine
	pre_entity_engine:client_event(EntityEngine, EntityID, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------

