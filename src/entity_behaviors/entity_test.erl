%%% @doc The test entity!

-module(entity_test).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([get_client_behavior/1, get_full_state/1, client_request/5, timer_fired/2]).

%% -------------------------------------------------------------------
%% API
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

timer_fired(_TimerRef, _Entity) ->
	ok.
