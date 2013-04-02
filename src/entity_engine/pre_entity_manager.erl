%%% @doc This is a simple wrapper interface to pre_entity_engine_sup to make it a bit cleaner to call, and a bit more
%%% intuitive what module is supposed to be used.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_manager).

-export([get_entity/1, create_entity/2, create_entity/3]).
-export([start_entity_engine/1]).

-include("log.hrl").
-include("pre_entity.hrl").

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets the entity record for the given id.
%%
%% This returns the current entity record for the entity of the given id.
-spec get_entity(EntityID :: binary()) ->
	{ok, Entity :: #entity{}} | not_found | {error, Msg :: string()}.

get_entity(EntityID) ->
	gen_server:call(pre_entity_engine_sup, {get_entity, EntityID}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity with the given behavior and definition.
%%
%% This creates a new entity, using the provided behavior and definition. This adds the entity to the least utilized
%% entity engine.
-spec create_entity(Behavior::atom(), Definition::json()) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(Behavior, Definition) ->
	Entity = #entity {
		behavior = Behavior,

		%TODO: What's a good default to use for model? undefined?
		model = proplists:get_value(<<"model">>, Definition, [{model, <<"Ships/ares">>}])
	},

	% Get the best entity engine to send this entity to, and then add it.
	EnginePid = gen_server:call(pre_entity_engine_sup, {get_best_engine, Entity}),
	pre_entity_engine:add_entity(EnginePid, Entity).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity with the given behavior and definition.
%%
%% This creates a new entity, using the provided behavior and definition, as well as setting the entity's client_info
%% field. This adds the entity to the least utilized entity engine.
-spec create_entity(Behavior::atom(), Definition::json(), ClientInfo::#client_info{}) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(Behavior, Definition, ClientInfo=#client_info{}) ->
	Entity = #entity {
		behavior = Behavior,
		client = ClientInfo,

		%TODO: What's a good default to use for model? undefined?
		model = proplists:get_value(<<"model">>, Definition, [{model, <<"Ships/ares">>}])
	},

	pre_entity_engine_sup:add_entity(Entity).

%% --------------------------------------------------------------------------------------------------------------------
%%
%% @doc Starts a new entity engine.
%%
%% This starts a new (supervised) entity engine, and adds it to our list of tracked entity engines.
-spec start_entity_engine(Args::list()) ->
	started | {failed, Reason::string()}.

start_entity_engine(Args) ->
	gen_server:cast(pre_entity_engine_sup, {start_entity_engine, Args}).
