%%% @doc pre_player_ship
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_player_ship).

-compile([{parse_transform, rec2json}]).

-define(transact(Thing), pre_data:transaction(fun() -> Thing end)).

-record(pre_player_ship, {
	id :: any(),                        % auto generated id
	name :: binary(),                   % beginning and trailing spaces stripped
	position :: vector:vec(),           % last known position in the world
	orientation :: quaternion:quat(),   % last known orientation in the world
	template :: binary(),               % the base ship template for this ship
	created :: erlang:timestamp(),
	updated :: erlang:timestamp()
}).

% API
-export([get_by_id/1, get_by_name/1, get_character/1, create/2, create/4, delete/1]).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Gets a player ship by id. Returns a player ship.
-spec get_by_id(ShipID :: binary()) -> {'ok', tuple()} | {error, term()}.
get_by_id(ShipID) ->
	?transact(pre_data:get_by_id(pre_player_ship, ShipID)).


%% @doc Gets a player ship by name. Returns a player ship.
-spec get_by_name(ShipName :: binary()) -> {'ok', tuple()} | {error, term()}.
get_by_name(ShipName) ->
	Got = ?transact(pre_data:search(pre_player_ship, [{name, ShipName}])),
	case Got of
		{ok, []} -> {error, notfound};
		{ok, [Ship]} -> {ok, Ship};
		Else -> Else
	end.

%% @doc Gets the character that owns this ship. Returns a character.
-spec get_character(Ship :: #pre_player_ship{}) -> {ok, term()} | {error, term()}.
get_character(Ship) ->
	Got = ?transact(pre_data:search(pre_player_ship, [{ship, Ship:id()}])),
	case Got of
		{ok, []} -> {error, notfound};
		{ok, [Char]} -> {ok, Char};
		{ok, [_ | _]} -> {error, duplicates};
		Else -> Else
	end.

%% @doc Creates a new player. Returns the newly created ship.
-spec create(Name :: binary(), Template :: binary()) -> {'ok', tuple()} | {error, term()}.
create(Name, Template) ->
	create(Name, undefined, undefined, Template).

%% @doc Creates a new player ship. Returns the newly created ship.
-spec create(Name :: binary(), Position :: tuple(), Orientation :: tuple(), Template :: binary()) -> {'ok', tuple()} | {error, term()}.
create(Name, Position, Orientation, Template) ->
	New = #pre_player_ship{
		name = Name,
		position = Position,
		orientation = Orientation,
		template = Template
	},

	?transact(pre_data:save(New)).

%% @doc Removed an character.
-spec delete(ShipID :: any()) -> 'ok'.
delete(ShipID) ->
	?transact(pre_data:delete(pre_player_ship, ShipID)).

%% ---------------------------------------------------------------------------------------------------------------------
