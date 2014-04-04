%%% @doc A helper module for working with characters.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_character).

-define(t(Thing), pre_data:transaction(fun() -> Thing end)).

% API
-export([get_by_id/1, get_by_name/1, get_by_account/1, create/5, create/6, delete/1]).

%% ---------------------------------------------------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Gets an character by id. Returns an character.
-spec get_by_id(CharacterID :: binary()) -> {'ok', tuple()}.
get_by_id(CharacterID) ->
	?t(pre_data:get_by_id(pre_rec_character, CharacterID)).


%% @doc Gets an character by name. Returns an character.
-spec get_by_name(CharacterName :: binary()) -> {'ok', tuple()}.
get_by_name(CharacterName) ->
	Got = ?t(pre_data:search(pre_rec_character, [{name, CharacterName}])),
	case Got of
		{ok, []} -> {error, notfound};
		{ok, [Char]} -> {ok, Char};
		{ok, [_ | _]} -> {error, duplicates}
	end.

%% @doc Gets characters by account name. Returns a list of characters.
-spec get_by_account(AccountID :: binary()) -> {'ok', list()}.
get_by_account(AccountID) ->
	?t(pre_data:search(pre_rec_character, [{account, AccountID}])).


%% @doc Creates a new level 1 character. Returns the newly created character.
-spec create(Name :: binary(), Account :: binary(), Race :: atom(), Faction :: atom(), Ship :: binary()) -> {'ok', tuple()} | {error, term()}.
create(Name, Account, Race, Faction, Ship) ->
	create(Name, Account, Race, Faction, Ship, 1).

%% FIXME: De-duplicate before creating!
%% @doc Creates a new character. Returns the newly created character.
-spec create(Name :: binary(), Account :: binary(), Race :: atom(), Faction :: atom(), Ship :: binary(), Level :: integer()) -> {'ok', tuple()} | {error, term()}.
create(Name, Account, Race, Faction, Ship, Level) ->
	New = pre_rec_character:new(Name, Account, Race, Faction, Ship, Level),
	?t(pre_data:save(New)).


%% @doc Removed an character.
-spec delete(CharacterID :: any()) -> 'ok'.
delete(CharacterID) ->
	?t(pre_data:delete(pre_rec_character, CharacterID)).


%% ---------------------------------------------------------------------------------------------------------------------