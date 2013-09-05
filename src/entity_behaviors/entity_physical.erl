%%% @doc An entity representing a physical object.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_physical).

-include("log.hrl").
-include("pre_entity.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/1, simulate/2, get_client_behavior/0, get_full_state/1, client_request/5, client_event/4,
	entity_event/3]).

-define(STEP_SIZE, 50).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(InitialEntity) ->
	% Pull out the initial physical state from the entity we were passed.
	Physical = case dict:find(physical, InitialEntity#entity.state) of
		{ok, Value} ->
			InitialPhysical = pre_physics_rk4:from_proplist(Value),
			pre_physics_rk4:update_from_proplist(InitialPhysical, [{last_update, os:timestamp()}]);
		error ->
			pre_physics_rk4:default_physical()
	end,

	State = dict:store(physical, Physical, dict:new()),

	% Return the updated entity
	InitialEntity#entity{
		state = State
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(Entity, _EntityEngineState) ->
	EntityState = Entity#entity.state,
	LastPhysical = dict:fetch(physical, EntityState),

	% Do physics simulation
	Physical = pre_physics_rk4:simulate(LastPhysical),

	% Save State
	NewState = dict:store(physical, Physical, EntityState),

	% Calculate the updated state
	entity_base:calc_update(NewState, Entity).

%% --------------------------------------------------------------------------------------------------------------------

get_client_behavior() ->
	<<"Physical">>.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(Entity) ->
	Physical = dict:fetch(physical, Entity#entity.state),

	[{physical, pre_physics_rk4:to_proplist(Physical)} | entity_base:get_full_state(Entity)].

%% --------------------------------------------------------------------------------------------------------------------

client_request(Entity, Channel, RequestType, RequestID, Request) ->
	entity_base:client_request(Entity, Channel, RequestType, RequestID, Request).

%% --------------------------------------------------------------------------------------------------------------------

client_event(Entity, Channel, EventType, Event) ->
	entity_base:client_event(Entity, Channel, EventType, Event).

%% --------------------------------------------------------------------------------------------------------------------

entity_event(Event, From, Entity) ->
	entity_physical:client_event(Event, From, Entity).
