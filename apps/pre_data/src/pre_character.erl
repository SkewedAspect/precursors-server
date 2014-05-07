%%% @doc A helper module for working with characters.

-module(pre_character).

-compile([{parse_transform, rec2json}]).

-define(transact(Thing), pre_data:transaction(fun() -> Thing end)).

-record(pre_character, {
	id :: any(),            % auto generated id
	name :: binary(),       % beginning and trailing spaces stripped; primary key
	account :: any(),       % ID of the account record this character belongs to.
	race :: atom(),         % limited choices, ex: [human, gikdaa, norael]
	faction :: atom(),      % limited choices, ex: [league, terran, freelance]
	ship :: any(),          % ID of ship record
	level :: integer(),     % the level of the character, range 1 to MaxLevel
	created :: erlang:timestamp(),
	updated :: erlang:timestamp()
}).

% API
-export([get_by_id/1, get_by_name/1, get_by_account/1, create/5, create/6, delete/1, save/1]).

%% ---------------------------------------------------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Gets an character by id. Returns a character.
-spec get_by_id(CharacterID :: binary()) -> {'ok', tuple()}.
get_by_id(CharacterID) ->
	?transact(pre_data:get_by_id(pre_character, CharacterID)).


%% @doc Gets an character by name. Returns an character.
-spec get_by_name(CharacterName :: binary()) -> {'ok', tuple()}.
get_by_name(CharacterName) ->
	Got = ?transact(pre_data:search(pre_character, [{name, CharacterName}])),
	case Got of
		{ok, []} -> {error, notfound};
		{ok, [Char]} -> {ok, Char};
		{ok, [_ | _]} -> {error, duplicates};
		Else -> Else
	end.

%% @doc Gets characters by account name. Returns a list of characters.
-spec get_by_account(AccountID :: binary()) -> {'ok', list()}.
get_by_account(AccountID) ->
	?transact(pre_data:search(pre_character, [{account, AccountID}])).


%% @doc Creates a new level 1 character. Returns the newly created character.
-spec create(Name :: binary(), Account :: binary(), Race :: atom(), Faction :: atom(), Ship :: binary()) -> {'ok', tuple()} | {error, term()}.
create(Name, Account, Race, Faction, Ship) ->
	create(Name, Account, Race, Faction, Ship, 1).


%% @doc Creates a new character. Returns the newly created character.
-spec create(Name :: binary(), Account :: binary(), Race :: atom(), Faction :: atom(), Ship :: binary(), Level :: integer()) -> {'ok', tuple()} | {error, term()}.
create(Name, Account, Race, Faction, Ship, Level) ->
	case get_by_name(Name) of
		{error, notfound} ->
			New = #pre_character{
				name = Name,
				account = Account,
				race = Race,
				faction = Faction,
				ship = Ship,
				level = Level
			},

			?transact(pre_data:save(New));
		{ok, _Char} ->
			{error, already_exists}
	end.


%% @doc Saves a character. Returns the character.
-spec save(Character :: #pre_character{}) -> {ok, #pre_character{}} | {error, term()}.
save(Character) ->
	?transact(pre_data:save(Character)).


%% @doc Removed an character.
-spec delete(CharacterID :: any()) -> 'ok'.
delete(CharacterID) ->
	?transact(pre_data:delete(pre_character, CharacterID)).


%% ---------------------------------------------------------------------------------------------------------------------
