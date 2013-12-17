%%% @doc This is a simple wrapper interface to pre_entity_engine_sup to make it a bit cleaner to call, and a bit more
%%% intuitive what module is supposed to be used.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_manager).

-export([get_entity/1, create_entity/2, create_entity/3, create_entity/4, load_entity/1]).
-export([start_entity_engine/1, get_full_update/1]).

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

%% @doc Creates a new entity with the given controller and definition.
%%
%% This creates a new entity, using the provided controller and definition. This does not attempt to load the entity from
%% the database.
-spec create_entity(Controller::atom(), Definition::json_object()) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(Controller, Definition) ->
	Entity = #entity {
		id = make_entity_id(),
		controller = Controller
	},

	% Populate the definition
	Entity1 = populate_definition(Entity, Definition),

	% Initialize the controller
	InitializedEntity = Controller:init(Entity1),

	pre_entity_engine_sup:add_entity(InitializedEntity),
	{ok, InitializedEntity}.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity with the given controller and definition.
%%
%% This creates a new entity, using the provided controller and definition, as well as setting the entity's client_info
%% field. This does not attempt to load the entity from the database.
-spec create_entity(Controller::atom(), Definition::json_object(), ClientInfo::#client_info{}) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()};

	(EntityID::binary(), Controller::atom(), Definition::json_object()) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(Controller, Definition, ClientInfo=#client_info{}) ->
	Entity = #entity {
		id = make_entity_id(),
		controller = Controller,
		client = ClientInfo
	},

	% Populate the definition
	Entity1 = populate_definition(Entity, Definition),

	% Initialize the controller
	InitializedEntity = Controller:init(Entity1),

	pre_entity_engine_sup:add_entity(InitializedEntity),
	{ok, InitializedEntity};

%% @doc Creates a new entity either loading from the db, or with the given controller and definition.
%%
%% This creates a new entity, attempting to load it from the database. If it is not found, it will create a new one
%% using the provided controller and definition.

create_entity(undefined, Controller, Definition) ->
	create_entity(Controller, Definition);

create_entity(EntityID, Controller, Definition) ->
	% Do a database lookup, and attempt to load the entity.
	InitialEntity = case pre_data:get(<<"entity">>, EntityID) of
		{ok, Value} ->
			json_to_entity(Value);
		notfound ->
			#entity{};
		{error, Error} ->
			?error("Error looking up entity ~p during creation. Error was: ~p",
				[EntityID, Error]),
			#entity{}
	end,

	% Set the entity's default controller
	Entity = InitialEntity#entity {
		id = make_entity_id(),
		controller = Controller
	},

	% Populate the definition
	Entity1 = populate_definition(Entity, Definition),

	% Initialize the controller
	InitializedEntity = Controller:init(Entity1),

	pre_entity_engine_sup:add_entity(InitializedEntity),
	{ok, InitializedEntity}.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity either loading from the db, or with the given controller and definition.
%%
%% This creates a new entity, attempting to load it from the database. If it is not found, it will create a new one
%% using the provided controller and definition.
-spec create_entity(EntityID::binary(), Controller::atom(), Definition::json_object(), ClientInfo::#client_info{}) ->
	{ok, Entity::#entity{}} | {failed, Reason :: string()} | {error, Msg :: string()}.

create_entity(undefined, Controller, Definition, ClientInfo) ->
	create_entity(Controller, Definition, ClientInfo);

create_entity(EntityID, Controller, Definition, ClientInfo) ->
	% Do a database lookup, and attempt to load the entity.
	InitialEntity = case pre_data:get(<<"entity">>, EntityID) of
		{ok, Value} ->
			json_to_entity(Value);
		notfound ->
			#entity{};
		{error, Error} ->
			?error("Error looking up entity ~p during creation. Error was: ~p",
				[EntityID, Error]),
			#entity{}
	end,

	% Set the entity's default controller
	Entity = InitialEntity#entity {
		id = make_entity_id(),
		controller = Controller,
		client = ClientInfo
	},

	% Populate the definition
	Entity1 = populate_definition(Entity, Definition),

	% Initialize the controller
	InitializedEntity = Controller:init(Entity1),

	pre_entity_engine_sup:add_entity(InitializedEntity),
	{ok, InitializedEntity}.
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Creates a new entity with the given controller and definition.
%%
%% This creates a new entity, using the provided controller and definition. This does not attempt to load the entity from
%% the database.
-spec load_entity(EntityID::binary()) ->
	{ok, Entity::#entity{}} | notfound | {error, Msg :: string()}.

load_entity(EntityID) ->
	% Do a database lookup, and attempt to load the entity.
	InitialEntity = case pre_data:get(<<"entity">>, EntityID) of
		{ok, Value} ->
			json_to_entity(Value);

		notfound ->
			notfound;

		{error, Error} ->
			?error("Error looking up entity ~p during creation. Error was: ~p",
				[EntityID, Error]),
			{error, Error}
	end,

	% Load it if we found it, otherwise return appropriately.
	case InitialEntity of
		notfound ->
			notfound;

		{error, Msg} ->
			{error, Msg};

		_ ->
			Controller = InitialEntity#entity.controller,

			% Initialize the controller
			InitializedEntity = Controller:init(InitialEntity),

			pre_entity_engine_sup:add_entity(InitializedEntity),
			{ok, InitializedEntity}
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts a new entity engine.
%%
%% This starts a new (supervised) entity engine, and adds it to our list of tracked entity engines.
-spec start_entity_engine(Args::list()) ->
	started | {failed, Reason::string()}.

start_entity_engine(Args) ->
	gen_server:cast(pre_entity_engine_sup, {start_entity_engine, Args}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts a new entity engine.
%%
%% This starts a new (supervised) entity engine, and adds it to our list of tracked entity engines.
-spec get_full_update(Entity :: #entity{}) ->
	FullUpdate :: json().

get_full_update(Entity) ->
	#entity{
		controller = Controller
	} = Entity,

	Controller:get_full_state(Entity).

%% --------------------------------------------------------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------------------------------------------------------

% Special case empty object.
json_to_dict([{}]) ->
	dict:new();

json_to_dict(JSON) ->
	dict:from_list(JSON).

% Convert from an entity json object to a entity record
json_to_entity(EntityJSON) ->
	#entity {
		id = proplists:get_value(id, EntityJSON),
		controller = proplists:get_value(controller, EntityJSON),
		state = proplists:get_value(state, EntityJSON)
}.

% Populates the state dict with the definition from the database; however since controller is not part of state,
% we need to pull it out and handle it seperately.
populate_definition(Entity, Definition) ->
	DefDict = json_to_dict(Definition),

	% Update the entity
	case dict:find(controller, DefDict) of
		{ok, Controller} ->
			dict:erase(controller, DefDict),

			Entity#entity{
				controller = Controller,
				state = DefDict
			};
		error ->
			Entity#entity{
				state = DefDict
			}
	end.


make_entity_id() ->
	%list_to_binary(ref_to_list(make_ref())).
	base64:encode(crypto:sha(term_to_binary({make_ref(), os:timestamp()}))).
