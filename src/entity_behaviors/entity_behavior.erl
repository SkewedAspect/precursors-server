%%% @doc The behavior for all of our entity behavior modules.
%%%
%%% `init(Entity)' is called when a new entity is created (or loaded from the database). It should setup the default
%%% state required by the behavior. It should return an updated Entity record.
%%%
%%% `simulate(Entity, EntityEngineState)' is called every simulation frame by the entity engine. It is expected
%%% to return `{Update, NewEntity}` where `Update` is either a JSON structure representing any changes or `undefined`
%%% if no changes occurred, and `NewEntity` is the new entity record that should be used for future simulation.
%%%
%%% `get_client_behavior()' is called to get the name of the client-side behavior that corresponds to this behavior
%%% module. It should return a binary..
%%%
%%% `get_full_state(Entity)' is called whenever a full JSON state message is required. It is expected to return
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
	[{init, 1}, {simulate, 2}, {get_client_behavior, 0}, {get_full_state, 1}, {client_request, 5}, {client_event, 4},
		{entity_event, 3}];

behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------------------------------------------

