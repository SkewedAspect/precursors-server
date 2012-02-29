-module(pre_client_hooks).

-include("log.hrl").
-include("pre_client.hrl").

-export([register_hooks/0, client_login_hook/2, client_ping_request_hook/5]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]),
	pre_hooks:add_hook(client_request_received, ?MODULE, client_ping_request_hook, undefined, [node()]).

%% ------------------------------------------------------------------

client_login_hook(undefined, ClientRecord) ->
	?debug("Client logged in: ~p", [ClientRecord]),
	#client_info{connection = Connection} = ClientRecord,
	LevelUrl = <<"zones/test/TestArea.json">>,
	LoadLevel = {struct, [
		{type, <<"setZone">>},
		{level, LevelUrl}
	]},
	pre_client_connection:send(Connection, tcp, event, level, LoadLevel),
	{ok, undefined}.

client_ping_request_hook(undefined, <<"ping">>, <<"ping">>, Envelope, ClientRecord) ->
	?debug("Servicing ping request! YAY!"),
	#client_info{connection = Connection} = ClientRecord,
	{MegaSecs, Secs, MicroSecs} = now(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
	PingResponse = {struct, [
		{confirm, true},
		{timestamp, Timestamp}
	]},
	#envelope{channel = Channel, id = MsgID} = Envelope,
	pre_client_connection:send(Connection, tcp, {response, MsgID}, Channel, PingResponse),
	{ok, undefined};

client_ping_request_hook(_HookInfo, _RequestType, _Channel, _Envelope, _ClientRecord) ->
	not_handled.
