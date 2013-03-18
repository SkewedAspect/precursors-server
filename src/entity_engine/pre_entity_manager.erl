%%% @doc The behavior for all of our entity behavior modules. Provides a basic interface that we can count on.

-module(pre_entity_manager).
-export([get/1, set/1]).

-include("log.hrl").
-include("pre_entity_new.hrl").

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets the entity record for the given id.
%%
%% This returns the current entity record for the entity of the given id.
-spec get(EntityID :: binary()) ->
	{ok, Entity :: #entity{}} | not_found | {error, Msg :: string()}.

get(_EntityID) ->
	{error, "Not implemented."}.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets the entity record.
%%
%% This sets the entity record for the passed in entity. This is a transaction; it will fail if an update has come in
%% since the entity was retrieved. Applying the update and restarting the transaction is the concern of the calling code.
-spec set(Entity :: binary()) ->
	ok | {failed, Reason :: string()} | {error, Msg :: string()}.

set(_Entity) ->
	{failed, "Not implemented."};

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sets the entity records.
%%
%% This sets the entity records to the passed in entities. This is a transaction; it will fail if an update has come in
%% since the entity was retrieved. Applying the update and restarting the transaction is the concern of the calling code.
-spec set([Entity :: binary()]) ->
	ok | {failed, Reason :: string()} | {error, Msg :: string()}.

set(_EntityList) ->
	{failed, "Not implemented."}.

