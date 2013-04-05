%%% @doc An entity representing a physical object.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_physical).

-include("log.hrl").
-include("pre_entity.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/1, simulate/2, get_full_state/1, client_request/5, client_event/5]).

-define(STEP_SIZE, 50).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(InitialEntity) ->
	% Pull out the initial physical state from the entity we were passed.
	InitialPhysical = case dict:find(physical, InitialEntity#entity.state) of
		{ok, Value} ->
			Value;
		error ->
			dict:new()
	end,

	% Set up default physical state
	DefaultPhysical = dict:from_list([
        { position, {0, 0, 0} },
        { linear_momentum, {0, 0, 0} },
        { orientation, {1, 0, 0, 0} },
        { angular_momentum, {0, 0, 0} },

        { force_absolute, {0, 0, 0} },
        { force_relative, {0, 0, 0} },
        { torque_absolute, {0, 0, 0} },
        { torque_relative, {0, 0, 0} },

        { linear_velocity, {0, 0, 0} },
        { angular_velocity, {0, 0, 0} },
        { spin, {1, 0, 0, 0} },

        { mass, 1 },
        { inverse_mass, 1 },
        { inertia_tensor, 1 },
        { inverse_inertia_tensor, 1 }
	]),

	% Merge our initial physical state dict with our default values, prefering our initials where there's
	% conflicts.
	Physical = dict:merge(fun(_Key, InitialVal, _DefaultVal) ->
		InitialVal
	end, InitialPhysical, DefaultPhysical),

	% Set last_update to now, since the initial load counts as an update. This prevents us from trying to simulate a
	% single step that's as long as the entity's been offline (in the case of entities loaded from the db).
	State = dict:store(physical, dict:store(last_update, os:timestamp(), Physical), dict:new()),

	% Return the initial entity
	InitialEntity#entity{
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
