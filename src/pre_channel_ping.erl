%% @doc Responds to ping requests.  Something else must register for the
%% client connection hook using {@link register_hooks/0}.

-module(pre_channel_ping).

-include("log.hrl").
-include("pre_client.hrl").

% Because this saves us _so_ much code.
-define(CHANNEL, <<"ping">>).

% api
-export([register_hooks/0]).

% hooks
-export([client_login_hook/2]).

% pre_client_channels
-export([client_request/4, client_response/4, client_event/3]).

%% -------------------------------------------------------------------
%% api
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

client_request(?CHANNEL, Client, Id, _Request, _Info) ->
	#client_info{connection = Connection} = Client,
	{MegaSecs, Secs, MicroSecs} = now(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
	PingResponse = {struct, [
		{confirm, true},
		{timestamp, Timestamp}
	]},
	pre_client_connection:send(Connection, tcp, {response, Id}, ?CHANNEL, PingResponse),
	{ok, []};

client_request(RequestType, Client, Id, Request, _Info) ->
	{ok, []}.

%% @hidden
client_response(Client, Id, Response, _Info) ->
	{ok, []}.

%% @hidden
client_event(Client, Event, _Info) ->
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
	?debug("Client ~p logged in; registering 'ping' channel.", [ClientRecord]),
	#client_info{channel_manager = ChannelManager} = ClientRecord,
	pre_client_channels:set_channel(ChannelManager, ?CHANNEL, ?MODULE, []),
	{ok, undefined}.
