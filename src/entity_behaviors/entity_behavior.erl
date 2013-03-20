%%% @doc The behavior for all of our entity behavior modules.
%%%
%%% `simulate(EntityRecord, EntityEngineState)` is called every simulation frame by the entity engine. It is expected
%%% to return a the new entity record that should be used for future simulation. (This is how we modify the entity
%%% engine's state to include the updated entity.)
%%% 
%%% `get_full_update(EntityRecord)` is called whenever a full JSON update message is required. It is expected to return
%%% a JSON structure representing the entity's current state.
%%%
%%% `client_request` is called by the entity/client interface (pre_entity_interface) when an request comes in that the
%%% behavior needs to process. It is expected that this return an appropriate response for the client.
%%%
%%% `client_event` is called by the entity/client interface (pre_entity_interface) when an event comes in that the
%%% behavior needs to process. No response is expected.
%%%
%%% --------------------------------------------------------------------------------------------------------------------

-module(entity_behavior).
-export([behaviour_info/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

behaviour_info(callbacks) ->
	[{simulate, 2}, {get_full_update, 1}, {client_request, 5}, {client_event, 5}];

behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------------------------------------------

