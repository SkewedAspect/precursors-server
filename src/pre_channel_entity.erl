%%% @doc The entity channel worker - forwards updates from nearby entities to the client.
%%%
%%% One worker process exists per client, and it just handles entity updates for that client.

-module(pre_channel_entity).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% Because this saves us _so_ much code.
-define(CHANNEL, entity).

% API
-export([start_link/0, client_connected/2, client_disconnected/3, client_inhabited_entity/3]).
-export([broadcast_event/3, build_state_event/4, send_update_for_entity/1]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(entity_event_type() :: 'full' | 'update' | 'create' | 'remove').
-export_type([entity_event_type/0]).

-record(state, {
	clients = [] :: [{pid(), #entity{} | undefined}]
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

client_inhabited_entity(Pid, ClientPid, EntityID) ->
	pre_entity_engine:get_entity_record_async(EntityID, fun (EntityDef) ->
		gen_server:cast(Pid, {client_inhabited_entity, ClientPid, EntityDef})
	end).

%% -------------------------------------------------------------------

broadcast_event(Pid, EntityID, Content) ->
	gen_server:cast(Pid, {entity_event, EntityID, Content}).

%% -------------------------------------------------------------------

build_state_event(EventType, StateUpdate, EntityID, Timestamp) ->
	#entity_id{
		engine = EntityEngine,
		ref = EntityRef
	} = EntityID,

	NetworkEntityID = [
		list_to_binary(erlang:pid_to_list(EntityEngine)),
		list_to_binary(erlang:ref_to_list(EntityRef))
	],

	[
		{type, EventType},
		{id, NetworkEntityID},
		{timestamp, Timestamp}
		| StateUpdate
	].

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
	?debug("Client ~p logged in; registering ~p channel.", [ClientInfo, ?CHANNEL]),
	#client_info{
		channel_manager = ChannelManager,
		connection = ConnectionPID
	} = ClientInfo,
	pre_client_channels:set_channel(ChannelManager, ?CHANNEL, ?MODULE, []),
	{noreply, State#state{clients = [{ConnectionPID, undefined} | Clients]}};

handle_cast({client_disconnected, ConnectionPID, Reason}, State) ->
	Clients = State#state.clients,
	?debug("Client process ~p disconnected for reason ~p; removing from list.", [ConnectionPID, Reason]),
    {noreply, State#state{clients = proplists:delete(ConnectionPID, Clients)}};

handle_cast({client_inhabited_entity, ConnectionPid, EntityDef}, State) ->
	% Update our list of clients, replacing the given client's entity.
	Clients = lists:keyreplace(ConnectionPid, 1, State#state.clients, {ConnectionPid, EntityDef}),
	pre_entity_engine:get_full_state_async(EntityDef,
		fun (Timestamp, FullUpdate) ->
			%FIXME: This should really be of type 'inhabit'...
			FullMessage = build_state_event(inhabit, FullUpdate, EntityDef#entity.id, Timestamp),
			pre_client_connection:send(ConnectionPid, udp, event, entity, FullMessage)
		end
	),

	Timer = timer:apply_interval(400, ?MODULE, send_update_for_entity, [EntityDef]),
	?info("Started entity full update event timer ~p for ~p.", [Timer, EntityDef#entity.id]),

	{noreply, State#state{clients = Clients}};

handle_cast({entity_event, EntityID, Content}, State) ->
	?debug("Broadcasting event for entity ~p: ~p", [EntityID, Content]),
	FullEvent = {struct, Content},
	%FIXME: Filter this so it only goes to clients within a certain distance!
	%(ClientEntity#entity.physical#physical.position)
	broadcast_event(FullEvent, State),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

send_update_for_entity(EntityDef) ->
	?debug("Sending update for entity ~p.", [EntityDef#entity.id]),
	pre_channel_entity_sup:broadcast_full_update(EntityDef).

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
		|| {Pid, _EntityDef} <- State#state.clients],
	State.
