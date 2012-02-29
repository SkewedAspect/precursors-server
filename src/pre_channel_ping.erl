%%% @doc Responds to ping requests.

-module(pre_channel_ping).

-include("log.hrl").
-include("pre_client.hrl").

% Because this saves us _so_ much code.
-define(CHANNEL, <<"ping">>).

% api
-export([register_hooks/0]).

% pre_client_channels
-export([client_request/4, client_response/4, client_event/3]).

%% -------------------------------------------------------------------
%% api
%% -------------------------------------------------------------------

register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, []).

%% -------------------------------------------------------------------
%% pre_client_channels
%% -------------------------------------------------------------------

client_request(Client, Id, Request, Info) ->
	client_request(request_type(Request), Client, Id, Request, Info).

client_request(<<"ping">>, Client, Id, _Request, _Info) ->
	?debug("Servicing ping request! YAY!"),
	#client_info{connection = Connection} = Client,
	{MegaSecs, Secs, MicroSecs} = now(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
	PingResponse = {struct, [
		{confirm, true},
		{timestamp, Timestamp}
	]},
	pre_client_connection:send(Connection, tcp, {response, Id}, ?CHANNEL, PingResponse),
	ok;

client_request(RequestType, Client, Id, Request, _Info) ->
	?debug("Ping channel received unrecognized ~p request! (~p, ~p, ~p)", [RequestType, Client, Id, Request]),
	ok.

client_response(Client, Id, Response, _Info) ->
	?debug("Ping channel received unrecognized response! (~p, ~p, ~p)", [Client, Id, Response]),
	ok.

client_event(Client, Event, _Info) ->
	?debug("Ping channel received unrecognized event! (~p, ~p)", [Client, Event]),
	ok.

%% -------------------------------------------------------------------

request_type({struct, Request}) ->
	proplists:get_value(<<"type">>, Request);

request_type(_) ->
	undefined.

%% -------------------------------------------------------------------

client_login_hook(undefined, ClientRecord) ->
	?debug("Client ~p logged in; registering 'ping' channel.", [ClientRecord]),
	#client_info{channel_manager = ChannelManager} = ClientRecord,
	pre_client_channels:set_sup_channel(ChannelManager, ?CHANNEL, ?MODULE, []).
