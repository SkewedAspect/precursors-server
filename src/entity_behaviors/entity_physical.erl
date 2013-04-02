%%% @doc An entity representing a physical object.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_physical).

-include("log.hrl").
-include("pre_entity.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/2, simulate/2, get_full_state/1, client_request/5, client_event/5]).

-define(STEP_SIZE, 50).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, Behavior) ->
	pre_entity_engine:start_entity_timer(EntityID, ?STEP_SIZE, do_physics),
	State = dict:new(),
	Physical = dict:new(),
	dict:store(physical, dict:store(last_update, os:timestamp(), Physical), State),
	#entity{
		id = EntityID,
		behavior = Behavior,
		state = State
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(Entity, _EntityEngineState) ->
	EntityState = Entity#entity.state,
	LastPhysical = dict:fetch(physical, EntityState),
	LastUpdate = dict:fetch(last_update, LastPhysical),
	ThisUpdate = os:timestamp(),

	% Do physics simulation
	Physical = pre_physics_rk4:simulate(timer:now_diff(ThisUpdate, LastUpdate) / 1000000, LastPhysical),

	% Calculate the updated state
	{Update, Entity1} = entity_base:calc_update(Physical, Entity),

	% Update last_updated
	Entity2 = Entity1#entity{
		state = dict:store(physical, dict:store(last_update, ThisUpdate, Physical), EntityState)
	},

	{Update, Entity2}.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(Entity) ->
	EntityState = dict:fetch(physical, Entity#entity.state),

	% Note, we start the accumulator with the behavior key for simplicity's sake.
	FullState = entity_base:gen_full_update(fun (Value) ->
		case Value of
			{_, _, _} ->
				vector:vec_to_list(Value);
			{_, _, _, _} ->
				quaternion:quat_to_list(Value);
			_ ->
				Value
			end
	end, [{behavior, <<"Physical">>}], EntityState),

	{FullState, Entity}.

%% --------------------------------------------------------------------------------------------------------------------

client_request(Entity, Channel, RequestType, RequestID, Request) ->
	entity_base:client_request(Entity, Channel, RequestType, RequestID, Request).

%% --------------------------------------------------------------------------------------------------------------------

client_event(Entity, ClientInfo, Channel, EventType, Event) ->
	entity_base:client_event(Entity, ClientInfo, Channel, EventType, Event).
