%%% @doc Responds to ping requests.  Something else must register for the
%%% client connection hook using {@link register_hooks/0}.

-module(pre_channel_ping).

-include("log.hrl").
-include("pre_client.hrl").

% api
-export([register_hooks/0]).

% hooks
-export([client_login_hook/2]).

% pre_client_channels
-export([client_request/4, client_response/4, client_event/3]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Add a hook to be triggered when a client logs in.  The hook adds
%% the ping channel to the client.
register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]).

%% -------------------------------------------------------------------
%% pre_client_channels
%% -------------------------------------------------------------------

%% @hidden
client_request(Client, Id, Request, Info) ->
	client_request(request_type(Request), Client, Id, Request, Info).

%% @hidden
client_response(_Client, _Id, _Response, _Info) ->
	{ok, []}.

%% @hidden
client_event(_Client, _Event, _Info) ->
	{ok, []}.

%% -------------------------------------------------------------------

%% @hidden
client_request(<<"ping">>, Client, Id, _Request, _Info) ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
	PingResponse = {struct, [
		{confirm, true},
		{timestamp, Timestamp}
	]},
	pre_client_connection:send(Client#client_info.connection, tcp,
		{response, Id}, <<"ping">>, PingResponse),
	{ok, []};

client_request(_RequestType, _Client, _Id, _Request, _Info) ->
	{ok, []}.

%% -------------------------------------------------------------------

%% @hidden
request_type({struct, Request}) ->
	proplists:get_value(<<"type">>, Request);

request_type(_) ->
	undefined.

%% -------------------------------------------------------------------

%% @doc The hook which adds the channel to the client.
client_login_hook(undefined, ClientRecord) ->
	?debug("Client ~p logged in; registering ~p channel.", [ClientRecord, <<"ping">>]),
	#client_info{channel_manager = ChannelManager} = ClientRecord,
	pre_client_channels:set_channel(ChannelManager, <<"ping">>, ?MODULE, []),
	{ok, undefined}.
