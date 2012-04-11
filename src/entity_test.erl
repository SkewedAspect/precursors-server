%%% @doc The test entity!

-module(entity_test).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([client_request/4, timer_fired/2]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

client_request(_RequestType, RequestID, _Request, Entity) ->
	ClientInfo = Entity#entity.client,
	Connection = ClientInfo#client_info.connection,
	Response = <<"Bumcovers.">>,
	pre_client_connection:send(Connection, tcp, {response, RequestID}, <<"input">>, Response),
	{ok, Response, Entity}.
	%{noreply, Entity}.

%% -------------------------------------------------------------------

timer_fired(TimerRef, Entity) ->
	ok.
