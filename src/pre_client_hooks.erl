-module(pre_client_hooks).

-include("log.hrl").
-include("pre_client.hrl").

-export([register_hooks/0, client_login_hook/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Registers a hook for when a client logs in.
-spec(register_hooks/0 :: () -> 'ok').
register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]).

%% ------------------------------------------------------------------

%% @doc The hook triggered on a client login. Tells a client to load a test zone.

-spec client_login_hook('undefined', ClientRecord :: #client_info{}) -> {'ok', 'undefined'}.

client_login_hook(undefined, ClientRecord) ->
	?debug("Client logged in: ~p", [ClientRecord]),
	Connection = ClientRecord#client_info.connection,
	LevelUrl = <<"zones/test/TestArea.json">>,
	LoadLevel = {struct, [
		{type, <<"setZone">>},
		{level, LevelUrl}
	]},
	pre_client_connection:send(Connection, tcp, event, level, LoadLevel),
	create_client_entity(ClientRecord),
	{ok, undefined}.

%% ------------------------------------------------------------------

create_client_entity(ClientRecord) ->
	Connection = ClientRecord#client_info.connection,
	?info("Creating entity for client ~p.", [ClientRecord]),
	pre_client_connection:set_inhabited_entity(Connection, pre_entity_engine_sup:create_entity(entity_ship)),
	{ok, undefined}.
