%%% @doc The entity channel - forwards messages from a client to its inhabited entity
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_channel_entity).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([register_hooks/0, build_state_event/3, generate_timestamp/0, generate_timestamp/1]).

% Hooks
-export([client_logged_in_hook/2]).

% pre_client_channels
-export([client_request/4, client_response/4, client_event/3]).


%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_logged_in_hook, undefined, [node()]).

%% --------------------------------------------------------------------------------------------------------------------

-spec build_state_event(EventType, StateUpdate, EntityID) -> json() when
	EventType :: binary(),
	StateUpdate :: json(),
	EntityID :: binary().

build_state_event(undefined, StateUpdate, EntityID) ->
	[
		{id, EntityID},
		{timestamp, generate_timestamp()}
		| StateUpdate
	];

build_state_event(EventType, StateUpdate, EntityID) ->
	build_state_event(undefined, [{type, EventType} | StateUpdate], EntityID).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Generates a timestamp for a network message.
%%
%% This just generates a (floating-point) number representing a number of seconds.

generate_timestamp() ->
	generate_timestamp(os:timestamp()).


generate_timestamp({MegaSecs, Secs, MicroSecs}) ->
	MegaSecs * 1000000 + Secs + MicroSecs / 1000000.

%% --------------------------------------------------------------------------------------------------------------------
%% Hooks
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Handle a client login hook call.
client_logged_in_hook(undefined, ClientInfo) ->
	?debug("Client ~p logged in; registering 'entity' channel.", [ClientInfo]),
	#client_info{
		channel_manager = ChannelManager
	} = ClientInfo,

	pre_client_channels:set_channel(ChannelManager, <<"entity">>, ?MODULE, []),
	{ok, undefined}.


%% --------------------------------------------------------------------------------------------------------------------
%% pre_client_channels
%% --------------------------------------------------------------------------------------------------------------------

client_request(#client_info{entity = undefined} = _ClientInfo, _RequestID, _Request, _Info) ->
	% INCREDIBLY NOISY:
	%?warning("Can't process 'entity' request ~p for client ~p; no entity inhabited!", [Request, ClientInfo]),
	Response = [
		{confirm, false},
		{reason, <<"No entity inhabited">>}
	],
	{reply, Response};

client_request(ClientInfo, RequestID, Request, _Info) ->
	RequestType = proplists:get_value(type, Request),
	Response = pre_entity_comm:client_request(ClientInfo, entity, RequestType, RequestID, Request),
	{reply, Response}.

%% --------------------------------------------------------------------------------------------------------------------

client_response(_Client, _Id, _Response, _Info) ->
	{ok, []}.

%% --------------------------------------------------------------------------------------------------------------------

client_event(ClientInfo, Event, _Info) ->
	EventType = proplists:get_value(type, Event),
	ok = pre_entity_comm:client_event(ClientInfo, entity, EventType, Event),
	{ok, []}.
