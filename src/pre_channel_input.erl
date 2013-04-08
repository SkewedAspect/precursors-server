%%% @doc The input channel - relays input events from the client to the appropriate entity.  Something else must
%%% register for the client connection hook using {@link register_hooks/0}.

-module(pre_channel_input).

-include("log.hrl").
-include("pre_client.hrl").

% API
-export([register_hooks/0]).

% Hooks
-export([client_login_hook/2]).

% pre_client_channels
-export([client_request/4, client_response/4, client_event/3]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]).

%% -------------------------------------------------------------------
%% Hooks
%% -------------------------------------------------------------------

client_login_hook(undefined, ClientInfo) ->
	?debug("Client ~p logged in; registering ~p channel.", [ClientInfo, <<"input">>]),
	ChannelManager = ClientInfo#client_info.channel_manager,
	pre_client_channels:set_channel(ChannelManager, <<"input">>, ?MODULE, []),
	{ok, undefined}.

%% -------------------------------------------------------------------
%% pre_client_channels
%% -------------------------------------------------------------------

client_request(#client_info{entity = undefined} = _ClientInfo, _RequestID, _Request, _Info) ->
	% INCREDIBLY NOISY:
	%?warning("Can't process 'input' request ~p for client ~p; no entity inhabited!", [Request, ClientInfo]),
	Response = [
		{confirm, false},
		{reason, <<"No entity inhabited">>}
	],
	{reply, Response};

client_request(ClientInfo, RequestID, Request, _Info) ->
	RequestType = proplists:get_value(type, Request),
	pre_entity_comm:client_request(ClientInfo, input, RequestType, RequestID, Request).

client_response(_Client, _Id, _Response, _Info) ->
	{ok, []}.

client_event(ClientInfo, Event, _Info) ->
	EventType = proplists:get_value(type, Event),
	pre_entity_comm:client_event(ClientInfo, input, EventType, Event).
