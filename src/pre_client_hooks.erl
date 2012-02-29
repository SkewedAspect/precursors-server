-module(pre_client_hooks).

-include("log.hrl").
-include("pre_client.hrl").

-export([register_hooks/0, client_login_hook/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

register_hooks() ->
	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_login_hook, undefined, [node()]).

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
