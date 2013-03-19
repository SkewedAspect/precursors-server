%%% @doc The behavior for all of our entity behavior modules. Provides a basic interface that we can count on.

-module(pre_entity_manager).

-export([get_entity/1, create_entity/2, create_entity/3, create_entity/4]).
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

get_entity(_EntityID) ->
	{error, "Not implemented."}.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity with the given behavior and definition.
%%
%% This creates a new entity, using the provided behavior and definition. This adds the entity to the least utilized
%% entity engine.
-spec create_entity(Behavior::atom(), Definition::json()) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(_Behavior, _Definition) ->
	{failed, "Not implemented."}.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity with the given behavior and definition.
%%
%% This creates a new entity, using the provided behavior and definition, as well as setting the entity's client_info
%% field. This adds the entity to the least utilized entity engine.
-spec create_entity(Behavior::atom(), Definition::json(), ClientInfo::#client_info{}) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()};

	%% @doc Creates a new entity with the given behavior and definition.
	%%
	%% This creates a new entity, using the provided behavior and definition. This adds the entity to the specified
	%% entity engine.
	(Behavior::atom(), Definition::json(), EntityEngine::pid()) ->
		{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(_Behavior, _Definition, _ClientInfo=#client_info{}) ->
	{failed, "Not implemented."};

create_entity(_Behavior, _Definition, _EnittyEngine) ->
	{failed, "Not implemented."}.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity with the given behavior and definition.
%%
%% This creates a new entity, using the provided behavior and definition, as well as setting the entity's client_info
%% field. This adds the entity to the specified entity engine.
-spec create_entity(Behavior::atom(), Definition::json(), ClientInfo::#client_info{}, EntityEngine::pid()) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(_Behavior, _Definition, _ClientInfo=#client_info{}, _EntityEngine) ->
	{failed, "Not implemented."}.

%% --------------------------------------------------------------------------------------------------------------------
%%
%% @doc Starts a new entity engine.
%%
%% This starts a new (supervised) entity engine, and adds it to our list of tracked entity engines.
-spec start_entity_engine(Args::list()) ->
	started | {failed, Reason::string()}.

start_entity_engine(_Args) ->
	{failed, "Not implemented."}.

%% --------------------------------------------------------------------------------------------------------------------

