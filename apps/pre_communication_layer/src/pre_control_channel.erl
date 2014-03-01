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
	Password= proplists:get_value(password, Request),

	lager:info("Authenticating user: ~p", [User]),

	%TODO: call into the new auth system.
	Confirm = false,
	Reason = "Not Implemented yet.",

	%TODO: set the current account, on success
	Account = {account, {}},

	LoginRep = [
		{confirm, Confirm},
		{reason, Reason},
		{cookie, State#client_state.cookie},
		{tcpPort, 6007}
	],

	% Send login response
	pre_client:send_response(self(), <<"control">>, ssl, ID, LoginRep),

	% Record login information in client_state
	AESKey = base64:decode(proplists:get_value(key, Request)),
	AESVector = base64:decode(proplists:get_value(vector, Request)),
	State#client_state{
		aes_key = AESKey,
		aes_vector = AESVector,
		account = Account
	};


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

	CharSelRep = case State#client_state.account of
		 undefined ->
			 [
				 {confirm, false},
				 {reason, "Not logged in."}
			 ];
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

			 %TODO: Look up the entity from cold storage, and load that into the entity event engine.
			 Entity = {entity, {}},

			 % Response object
			 [
				 {confirm, true}
			 ]
		end,

	% Send the response.
	pre_client:send_response(self(), <<"control">>, ssl, ID, CharSelRep),

	State#client_state{
		character = Character,
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