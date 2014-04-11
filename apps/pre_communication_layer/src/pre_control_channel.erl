%%% @doc Callbacks for the "control" channel.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_control_channel).

-behaviour(pre_gen_channel).

-include("pre_client.hrl").

% API
-export([handle_request/4, handle_response/4, handle_event/3]).

%% ---------------------------------------------------------------------------------------------------------------------

handle_request(<<"login">>, ID, Request, State) ->
	lager:info("Starting authentication"),

	User = proplists:get_value(user, Request),
	Password = proplists:get_value(password, Request),

	lager:info("Authenticating user: ~p", [User]),


	{LoginRep, State1} = case pre_account:authenticate(User, Password) of
		{error, Error} ->
			lager:info("Failed to authenticate user ~p", [User]),
			{
				[
					{confirm, false},
					{reason, Error}
				],
				State
			};
		ok ->
			lager:info("Successfully authenticated user ~p", [User]),

			% Record login information in client_state
			AESKey = base64:decode(proplists:get_value(key, Request)),
			AESVector = base64:decode(proplists:get_value(vector, Request)),
			{ok, Account} = pre_account:get_by_email(User),

			% Build response object for the client
			{
				[
					{confirm, true},
					{cookie, State#client_state.cookie},
					{tcpPort, 6007}
				],
				State#client_state{
					aes_key = AESKey,
					aes_vector = AESVector,
					account = Account
				}
			}
	end,

	% Send login response
	pre_client:send_response(self(), ssl, <<"control">>, ID, LoginRep),
	State1;


handle_request(<<"getCharacters">>, ID, _Request, State) ->
	lager:info("Retrieving character list for client ~p.", [self()]),

	GetCharsRep = case State#client_state.account of
		undefined ->
			[
				{confirm, false},
				{reason, "Not logged in."}
			];
		Account ->
			% Get list of characters
			{ok, Characters} = pre_character:get_by_account(Account:id()),

			% Response object
			[
				{confirm, true},
				{characters, [Character:to_json() || Character <- Characters]}
			]
	end,

	% Send the response
	pre_client:send_response(self(), ssl, <<"control">>, ID, GetCharsRep),
	State;


handle_request(<<"selectCharacter">>, ID, Request, State) ->
 	CharId = proplists:get_value(character, Request),
 	lager:info("Character selected: ~p", [CharId]),

	{SelectedChar, CharSelResp} = case State#client_state.account of
		 undefined ->
			 {undefined, [
				 {confirm, false},
				 {reason, "Not logged in."}
			 ]};
		 _Account ->
			 % Look up the Character
			 {ok, Character} = pre_character:get_by_id(CharId),

			 %TODO: Look up the correct level the character is located in. (For now, there's only one level.)
			 LevelUrl = <<"zones/test/TestArea.json">>,
			 LoadLevel = [
				 {type, <<"setZone">>},
				 {level, LevelUrl}
			 ],

			 % Tell the client to load the appropriate level.
			 pre_client:send_event(self(), <<"level">>, LoadLevel),

			 % Response object
			 {Character, [
				 {confirm, true}
			 ]}
		end,

	% Send the response.
	pre_client:send_response(self(), ssl, <<"control">>, ID, CharSelResp),

	% If we have a ship id, look it up. Otherwise, create a new one.
	Got = case SelectedChar:ship() of
		undefined ->
			%TODO: The player should create their ship when they don't have one, but this involves work on the client.
			% for now, we can simply make a new ship for them, with an arbitrary name.
			Name = SelectedChar:name(),
			pre_player_ship:create(<<Name/binary, "'s Ship">>, <<"Test Ship">>);
		ID ->
			% Look up the entity from cold storage
			pre_player_ship:get_by_id(ID)
	end,

	% Make some note of possible errors.
	Ship = case Got of
		{error, Reason} ->
			%TODO: We should be handling this!
			lager:error("Error encountered looking up (or creating) the ship! Reason: \"~p\" Character: ~p", [Reason, SelectedChar]),
			undefined;
	    {ok, Ship1} -> Ship1
	end,

	%TODO: pull the callback module from ship's template.
	pre_entity_balancer:add_entity(pre_ship_controller, Ship:id(), [Ship]),

	State#client_state{
		character = SelectedChar,
		entity = Ship
	};


handle_request(Type, ID, Request, State) ->
	lager:warning("[Control] Unknown Request: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_response(Type, ID, Request, State) ->
	lager:warning("[Control] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_event(<<"logout">>, _Request, _State) ->
	lager:info("Got logout event from client."),

	%TODO: We should probably let something know about the exit?
	lager:debug("Client exiting."),

	exit(normal);

handle_event(Type, Request, State) ->
	lager:warning("[Control] Unknown Event: ~p, ~p, ~p", [Type, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------
