%%% @doc The test entity!

-module(entity_test).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([get_client_behavior/1, get_full_state/1, client_request/4, timer_fired/2]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

get_client_behavior(_Entity) ->
	<<"Physical">>.

get_full_state(Entity) ->
	{BaseState, Entity1} = entity_base:get_full_state(Entity),
	{PhysicalState, Entity2} = entity_physical:get_full_state(Entity1),

	FullState = BaseState ++ PhysicalState,

	{FullState, Entity2}.

%% -------------------------------------------------------------------

client_request(_RequestType, RequestID, _Request, Entity) ->
	ClientInfo = Entity#entity.client,
	Connection = ClientInfo#client_info.connection,
	Response = <<"Bumcovers.">>,
	pre_client_connection:send(Connection, tcp, {response, RequestID}, <<"input">>, Response),
	{ok, Response, Entity}.
	%{noreply, Entity}.

%% -------------------------------------------------------------------

timer_fired(_TimerRef, _Entity) ->
	ok.
