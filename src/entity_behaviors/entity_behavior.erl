%%% @doc The behavior for all of our entity behavior modules.
%%%
%%% `simulate(Entity, EntityEngineState)' is called every simulation frame by the entity engine. It is expected
%%% to return `{Update, NewEntity}` where `Update` is either a JSON structure representing any changes or `undefined`
%%% if no changes occurred, and `NewEntity` is the new entity record that should be used for future simulation.
%%%
%%% `get_full_update(Entity)' is called whenever a full JSON update message is required. It is expected to return
%%% a JSON structure representing the entity's current state.
%%%
%%% `client_request(Channel, RequestType, RequestID, Request, Entity)' is called by the entity/client communication
%%% interface (pre_entity_comm) when the client sends a request that the behavior needs to process. It is expected to
%%% return `{ok, Response, NewEntity}` where `Response` is a JSON structure to send back to the client.
%%%
%%% `client_event(ClientInfo, Channel, EventType, Event, Entity)' is called by the entity/client communication
%%% interface (pre_entity_comm) when the client sends an event that the behavior needs to process. No response is
%%% expected.
%%%
%%% `entity_event(Event, From, Entity)' is called by the entity engine whenever another entity sends or broadcasts an
%%% event. No response is expected.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_behavior).
-export([behaviour_info/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

behaviour_info(callbacks) ->
	[{simulate, 2}, {get_full_state, 1}, {client_request, 5}, {client_event, 5}, {entity_event, 3}];

behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------------------------------------------

