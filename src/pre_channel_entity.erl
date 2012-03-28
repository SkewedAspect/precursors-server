%%% @doc The entity channel worker - forwards updates from nearby entities to the client.
%%%
%%% One worker process exists per client, and it just handles entity updates for that client.

-module(pre_channel_entity).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(position() :: #vector{}).
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
	{noreply, State#state{clients = [{ConnectionPID, {#vector{}, EntityID}} | Clients]}};

handle_cast({client_disconnected, ClientPid, Reason}, State) ->
	Clients = State#state.clients,
	?debug("Client process ~p disconnected for reason ~p; removing from list.", [ClientPid, Reason]),
    {noreply, State#state{clients = proplists:delete(ClientPid, Clients)}};

handle_cast({entity_event, Stuff}, State) ->
	?info("Entity event: ~p", [Stuff]),
	broadcast_message(Stuff, State),
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

broadcast_message(Message, State) ->
	FullMessage = {struct, [{type, entity_update} | Message]},
	[pre_client_connection:send(Pid, udp, event, entity, Message)
		|| {Pid, _EntityInfo} <- State#state.clients],
	State.
