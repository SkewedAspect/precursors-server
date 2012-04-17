%%% @doc The entity channel worker - forwards updates from nearby entities to the client.
%%%
%%% One worker process exists per client, and it just handles entity updates for that client.

-module(pre_channel_entity).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([start_link/0, client_connected/2, client_disconnected/3, broadcast_event/3]).
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

broadcast_event(Pid, EntityID, Content) ->
	gen_server:cast(Pid, {entity_event, EntityID, Content}).

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

%TODO: Do we actually have a need for these?
%handle_cast({client_connected, ClientInfo}, State) ->
%	Clients = State#state.clients,
%	?debug("Client ~p connected; adding to list.", [ClientInfo]),
%	#client_info{
%		connection = ConnectionPID,
%		entity = EntityID
%	} = ClientInfo,
%	{noreply, State#state{clients = [{ConnectionPID, undefined} | Clients]}};
%
%handle_cast({client_disconnected, ConnectionPID, Reason}, State) ->
%	Clients = State#state.clients,
%	?debug("Client process ~p disconnected for reason ~p; removing from list.", [ConnectionPID, Reason]),
%    {noreply, State#state{clients = proplists:delete(ConnectionPID, Clients)}};

handle_cast({client_inhabited_entity, ConnectionPid, EntityDef}, State) ->
	% Update our list of clients, replacing the given client's entity.
	Clients = lists:keystore(ConnectionPid, 1, State#state.clients, {ConnectionPid, EntityDef}),
	pre_entity_engine_sup:get_full_update_async(EntityDef,
		fun (FullUpdate) ->
			FullMessage = {struct, [{type, inhabit} | FullUpdate]},
			pre_client_connection:send(ConnectionPid, udp, event, entity, FullMessage)
		end
	),
	{noreply, State#state{clients = Clients}};

handle_cast({entity_event, EntityID, Content}, State) ->
	?debug("Broadcasting event for entity ~p: ~p", [EntityID, Content]),
	FullEvent = {struct, Content},
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
		|| {Pid, _EntityDef} <- State#state.clients],
	State.
