%%% @doc pre_rec_character
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_rec_character).

-compile([{parse_transform, rec2json}]).

% API
-export([new/6]).

-record(pre_rec_character, {
	id :: any(),            % auto generated id
	name :: binary(),       % beginning and trailing spaces stripped; primary key
	account :: any(),       % ID of the account record this character belongs to.
	race :: atom(),         % limited choices, ex: [human, gikdaa, norael]
	faction :: atom(),      % limited choices, ex: [league, terran, freelance]
	ship :: any(),          % ID of ship record
	level :: integer(),     % the level of the character, range 1 to MaxLevel
	created,
	updated
}).

%% ---------------------------------------------------------------------------------------------------------------------

new(Name, Account, Race, Faction, Ship, Level) ->
	#pre_rec_character{
		name = Name,
		account = Account,
		race = Race,
		faction = Faction,
		ship = Ship,
		level = Level
	}.

%% ---------------------------------------------------------------------------------------------------------------------