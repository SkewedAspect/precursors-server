%% @doc Dictates the background content loaded by the client, and dispatches events for entities in a given zone. (?)

-module(pre_channel_level).
-behavior(gen_server).

-include("log.hrl").

% gen_server
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
% pre_client_channels
-export([init/1, client_request/3, client_response/4, client_event/3]).

%% -------------------------------------------------------------------
%% api
%% -------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ch3}, ch3, [{wtf_should_go_here, wtf_should_go_here, wtf_should_go_here}], []).

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

init({_ChannelManager, _Module, _Info} = State) ->
	{ok, State}.

%% -------------------------------------------------------------------

client_request(Client, Request, Info) ->
	{ok, Info}.

client_response(Client, Id, Response, Info) ->
	{ok, Info}.

client_event(Client, Event, Info) ->
	{ok, Info}.

%% -------------------------------------------------------------------

%TODO: WTF do we do with these? Should these be getting casts and calls from client_*?
handle_call(request, _From, Chs) ->
    {reply, something, Chs}.

handle_cast({event, _Ch}, Chs) ->
    {noreply, Chs}.
