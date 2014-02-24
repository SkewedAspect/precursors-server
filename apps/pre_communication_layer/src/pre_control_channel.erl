%%% @doc pre_control_channel
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_control_channel).

-behaviour(pre_channel).

-include("pre_client.hrl").

% API
-export([handle_request/2, handle_response/2, handle_event/2]).

%% ---------------------------------------------------------------------------------------------------------------------

handle_request({<<"login">>, ID, Request}, State) ->
	lager:info("starting authentication"),

	% ---------------------------------------------------------------------------------------
	%TODO: This all needs to change for the new authentication system!

	% Check with authentication backends
	UserOrNick = proplists:get_value(user, Request),
	Password= proplists:get_value(password, Request),
	lager:info("Authenticating user: ~p", [UserOrNick]),
	{Confirm, ReasonOrUsername} = case pre_gen_auth:authenticate(UserOrNick, Password) of
		{allow, Username} ->
			{true, Username};
		{deny, Msg} ->
			{false, list_to_binary(Msg)};
		_ ->
			lager:warning("Authentication failed for unkown reason."),
			{false, <<"An unkown error has occured.">>}
	end,
	% ---------------------------------------------------------------------------------------

	LoginRep = [
		{confirm, Confirm},
		{reason, ReasonOrUsername},
		{cookie, State#client_state.cookie},
		{tcpPort, 6007}
	],

	% Send login response
	pre_client:send_response(self(), <<"control">>, ID, LoginRep),

	% Record login information in client_state
	AESKey = base64:decode(proplists:get_value(key, Request)),
	AESVector = base64:decode(proplists:get_value(vector, Request)),
	State#client_state{
		aes_key = AESKey,
		aes_vector = AESVector
	};

handle_request({<<"getCharacters">>, ID, Request}, State) ->
	%TODO: Convert this!

%% 	lager:info("Retrieving character list for client ~p.", [State#client_state.client_info]),
%%
%% 	%TODO: Make sure that Username is the Key for account, and not a secondary index, like nick name.
%% 	ClientInfo = State#client_state.client_info,
%% 	Username = ClientInfo#client_info.username,
%%
%% 	% Lookup the account
%% 	{ok, _Account, AccountMeta} = pre_data:get_with_meta(<<"account">>, Username),
%% 	Links = riakc_obj:get_all_links(AccountMeta),
%% 	CharacterLinks = proplists:get_value(<<"character">>, Links),
%%
%% 	% Fetch all charcters
%% 	Characters = fetch_characters(CharacterLinks),
%%
%% 	GetCharsRep = [
%% 		{confirm, true},
%% 		{characters, Characters}
%% 	],
%% 	respond(ssl, MessageID, <<"control">>, GetCharsRep),
	State;

handle_request({<<"selectCharacter">>, ID, Request}, State) ->
	%TODO: Convert this!

%% 	CharId = proplists:get_value(character, Request),
%% 	lager:info("Character selected: ~p", [CharId]),
%%
%% 	% Get the character.
%% 	Character = pre_data:get(<<"character">>, CharId),
%%
%% 	% Store character and id in client_state.
%% 	NewState = State#client_state{
%% 		client_info = State#client_state.client_info#client_info{
%% 			character_id = CharId,
%% 			character = Character
%% 		}
%% 	},
%%
%%
%% 	Connection = State#client_state.client_info#client_info.connection,
%% 	LevelUrl = <<"zones/test/TestArea.json">>,
%% 	LoadLevel = [
%% 		{type, <<"setZone">>},
%% 		{level, LevelUrl}
%% 	],
%% 	send(Connection, tcp, event, level, LoadLevel),
%%
%% 	%TODO: Look up existing entity, if possible.
%% 	EntityID = undefined,
%%
%% 	lager:info("Creating entity for client ~p.", [State#client_state.client_info]),
%% 	{ok, _Entity} = pre_entity_manager:create_entity(EntityID, entity_ship, [{}], State#client_state.client_info),
%%
%% 	CharSelRep = [
%% 		{confirm, true}
%% 	],
%% 	respond(ssl, MessageID, <<"control">>, CharSelRep),
%% 	NewState;
	State;

handle_request({Type, ID, Request}, State) ->
	lager:warning("[Control] Unknown Request: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_response({Type, ID, Request}, State) ->
	lager:warning("[Control] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_event({<<"logout">>, _ID, _Request}, _State) ->
	lager:info("Got logout event from client."),

	%TODO: We should probably let something know about the exit?

	exit(normal);

handle_event({Type, ID, Request}, State) ->
	lager:warning("[Control] Unknown Event: ~p, ~p, ~p", [Type, ID, Request]),
	State.