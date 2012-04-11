%%% @doc The entity channel worker - forwards updates from nearby entities to the client.
%%%
%%% One worker process exists per client, and it just handles entity updates for that client.

-module(pre_channel_entity).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([start_link/0, client_connected/2, client_disconnected/3, send_event/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(entity_event_type() :: 'full' | 'update' | 'create' | 'remove').
-export_type([entity_event_type/0]).

-type(position() :: vector:vec()).
-type(entity_info() :: {position(), entity_id()}).

-record(state, {
	clients = [] :: [{pid(), entity_info()}]
}).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% -------------------------------------------------------------------

client_connected(Pid, ClientInfo) ->
	gen_server:cast(Pid, {client_connected, ClientInfo}).

%% -------------------------------------------------------------------

client_disconnected(Pid, ClientPid, Reason) ->
	gen_server:cast(Pid, {client_disconnected, ClientPid, Reason}).

%% -------------------------------------------------------------------

send_event(Pid, Type, Content) ->
	gen_server:cast(Pid, {entity_event, Type, Content}).

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

init([]) ->
	State = #state{},
	{ok, State}.

%% -------------------------------------------------------------------

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

handle_cast({client_connected, ClientInfo}, State) ->
	Clients = State#state.clients,
	?debug("Client ~p connected; adding to list.", [ClientInfo]),
	#client_info{
		connection = ConnectionPID,
		entity = EntityID
	} = ClientInfo,
	{noreply, State#state{clients = [{ConnectionPID, {{0, 0, 0}, EntityID}} | Clients]}};

handle_cast({client_disconnected, ClientPid, Reason}, State) ->
	Clients = State#state.clients,
	?debug("Client process ~p disconnected for reason ~p; removing from list.", [ClientPid, Reason]),
    {noreply, State#state{clients = proplists:delete(ClientPid, Clients)}};

handle_cast({entity_event, Type, Content}, State) when Type == full; Type == update; Type == create; Type == remove ->
	?debug("'~p' entity event: ~p", [Type, Content]),
	FullEvent = {struct, [{type, Type} | Content]},
	broadcast_event(FullEvent, State),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

handle_info(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% -------------------------------------------------------------------

broadcast_event(FullMessage, State) ->
	[pre_client_connection:send(Pid, udp, event, entity, FullMessage)
		|| {Pid, _EntityInfo} <- State#state.clients],
	State.
