%%% @doc pre_rec_character
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_rec_character).

-compile([{parse_transform, rec2json}]).

-record(character, {
	id :: any(),            % auto generated id
	name :: binary(),       % beginning and trailing spaces stripped; primary key
	account :: binary(),    % ID of the account record this character belongs to.
	race :: atom(),         % limited choices, ex: [human, gikdaa, norael]
	faction :: atom(),      % limited choices, ex: [league, terran, freelance]
	ship :: binary(),       % ID of ship record
	level :: integer(),     % the level of the character, range 1 to MaxLevel
	created,
	updated
}).

% API
-export([new/6]).

%% ---------------------------------------------------------------------------------------------------------------------

new(Name, Account, Race, Faction, Ship, Level) ->
	#character{
		name = Name,
		account = Account,
		race = Race,
		faction = Faction,
		ship = Ship,
		level = Level
	}.

%% ---------------------------------------------------------------------------------------------------------------------