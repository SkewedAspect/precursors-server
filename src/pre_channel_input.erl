%%% @doc The input channel - relays input events from the client to the appropriate entity.  Something else must
%%% register for the client connection hook using {@link register_hooks/0}.

-module(pre_channel_input).

-include("log.hrl").
-include("pre_client.hrl").

% api
-export([register_hooks/0]).

% hooks
-export([client_login_hook/2]).

% pre_client_channels
-export([client_request/4, client_response/4, client_event/3]).

%% -------------------------------------------------------------------
%% api
%% -------------------------------------------------------------------

register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]).

%% -------------------------------------------------------------------
%% pre_client_channels
%% -------------------------------------------------------------------

client_request(ClientInfo, RequestID, Request, _Info) ->
	#client_info{
		entity = EntityID
	} = ClientInfo,
	pre_entity_engine:client_request(EntityID, ClientInfo, input, RequestID, Request).

client_response(_Client, _Id, _Response, _Info) ->
	{ok, []}.

client_event(ClientInfo, Event, _Info) ->
	#client_info{
		entity = EntityID
	} = ClientInfo,
	pre_entity_engine:client_event(EntityID, ClientInfo, input, Event).

%% -------------------------------------------------------------------

client_login_hook(undefined, ClientInfo) ->
	?debug("Client ~p logged in; registering ~p channel.", [ClientInfo, <<"input">>]),
	ChannelManager = ClientInfo#client_info.channel_manager,
	pre_client_channels:set_channel(ChannelManager, <<"input">>, ?MODULE, []),
	{ok, undefined}.
