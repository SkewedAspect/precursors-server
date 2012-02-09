%% @doc Dictates the background content loaded by the client, and dispatches events for entities in a given zone. (?)

-module(pre_channel_level).
-behavior(pre_client_channels).

-include("log.hrl").

% pre_client_channels
-export([init/1, client_request/3, client_response/4, client_event/3]).

%% -------------------------------------------------------------------
%% api
%% -------------------------------------------------------------------

%TODO: Needed?
start_link() ->
	gen_wtf:start_link().

%% -------------------------------------------------------------------
%% pre_channel_level
%% -------------------------------------------------------------------

%TODO: How should this actually work with pre_client_channels? Who calls init, what do they call it with, etc.
init({Args, _}) ->
	init(Args);

init({_ChannelManager, _Module, _Info} = State) ->
	{ok, State}.

%% -------------------------------------------------------------------

client_request(Client, Request, Info) ->
	{ok, Info}.

client_response(Client, Id, Response, Info) ->
	{ok, Info}.

client_event(Client, Event, Info) ->
	{ok, Info}.
