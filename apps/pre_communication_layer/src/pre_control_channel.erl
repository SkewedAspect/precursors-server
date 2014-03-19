%%% @doc Callbacks for the "control" channel.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_control_channel).

-behaviour(pre_gen_channel).

-include("pre_client.hrl").

% API
-export([handle_request/4, handle_response/4, handle_event/4]).

%% ---------------------------------------------------------------------------------------------------------------------

handle_request(<<"login">>, ID, Request, State) ->
	lager:info("Starting authentication"),

	User = proplists:get_value(user, Request),
	Password = proplists:get_value(password, Request),

	lager:info("Authenticating user: ~p", [User]),


	{LoginRep, State1} = case pre_account:authenticate(User, Password) of
		{error, Error} ->
			{[
				{confirm, false},
				{reason, Error}
			], State};
		ok ->
			% Record login information in client_state
			AESKey = base64:decode(proplists:get_value(key, Request)),
			AESVector = base64:decode(proplists:get_value(vector, Request)),
			{ok, Account} = pre_account:get_by_email(User),
			{[
				{confirm, true},
				{cookie, state#client_state.cookie},
				{tcpport, 6007}
			],
				State#client_state{
					aes_key = AESKey,
					aes_vector = AESVector,
					account = Account
				}}
	end,

	% Send login response
	pre_client:send_response(self(), <<"control">>, ssl, ID, LoginRep),
	State1.


handle_request(<<"getCharacters">>, ID, _Request, State) ->
	lager:info("Retrieving character list for client ~p.", [self()]),

	GetCharsRep = case State#client_state.account of
		undefined ->
			[
				{confirm, false},
				{reason, "Not logged in."}
			];
		Account ->
			%TODO: Get list of characters!
			Characters = [],

			% Response object
			[
				{confirm, true},
				{characters, Characters}
			]
	end,

	% Send the response
	pre_client:send_response(self(), <<"control">>, ssl, ID, GetCharsRep),
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
		 Account ->
			 %TODO: Look up the Character!
			 Character = {character, {}},

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
	pre_client:send_response(self(), <<"control">>, ssl, ID, CharSelResp),

	%TODO: Look up the entity from cold storage, and load that into the entity event engine.
	Entity = {entity, {}},

	State#client_state{
		character = SelectedChar,
		entity = Entity
	};


handle_request(Type, ID, Request, State) ->
	lager:warning("[Control] Unknown Request: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_response(Type, ID, Request, State) ->
	lager:warning("[Control] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_event(<<"logout">>, _ID, _Request, _State) ->
	lager:info("Got logout event from client."),

	%TODO: We should probably let something know about the exit?

	exit(normal);

handle_event(Type, ID, Request, State) ->
	lager:warning("[Control] Unknown Event: ~p, ~p, ~p", [Type, ID, Request]),
	State.