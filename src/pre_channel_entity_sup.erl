%%% @doc The entity channel - forwards updates from nearby entity to the client.
%%%
%%% This module is the supervisor for pre_channel_entity; only one of these should exist per node.

-module(pre_channel_entity_sup).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% Because this saves us _so_ much code.
-define(CHANNEL, <<"entity">>).

% gen_server
-export([start_link/1, broadcast_update/2, broadcast_full_update/1, broadcast_event/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([client_logged_in_hook/2, client_disconnect_hook/3, client_inhabited_entity_hook/3, fake_update/1]).

% pre_client_channels
-export([client_request/4]).

-type(workers_option() :: {'workers', non_neg_integer()}).
-type(start_option() :: workers_option()).
-type(start_options() :: [start_option()] | 'supervisor_start').

-record(state, {
	supervisor_pid :: pid(),
	worker_pids :: [pid()],
	client_count = 0 :: non_neg_integer()
}).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the entity channel master server.

-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% -------------------------------------------------------------------

client_request(Client, RequestID, Request, _Info) ->
	#client_info{
		entity = EntityID,
		connection = Connection
	} = Client,
	pre_entity_engine:client_request(EntityID, Connection, ?CHANNEL, RequestID, Request).

%% -------------------------------------------------------------------

-spec broadcast_update(EntityID :: #entity_id{}, StateDelta :: [{atom(), term()}]) -> 'ok'.

broadcast_update(EntityID, StateDelta) ->
	Content = [
		{state, {struct, StateDelta}}
	],
	broadcast_event(update, EntityID, Content).

%% -------------------------------------------------------------------

%% @doc Broadcast a full update for the given entity to all eligible clients.

-spec broadcast_full_update(Entity :: #entity{}) -> 'ok'.

broadcast_full_update(Entity) ->
	pre_entity_engine:get_full_state_async(Entity, fun (Timestamp, FullState) ->
		begin
			#entity{
				id = EntityID,
				model_def = ModelDef
			} = Entity,

			Content = [
				{modelDef, {struct, ModelDef}},
				{state, {struct, FullState}}
			],
			broadcast_event(full, EntityID, Content, Timestamp)
		end
	end).

%% -------------------------------------------------------------------

-spec broadcast_event(EventType, EntityID, EventContents) -> 'ok' when
	EventType :: pre_channel_entity:entity_event_type(),
	EntityID :: #entity_id{},
	EventContents :: [{atom(), term()}].

broadcast_event(EventType, EntityID, EventContents) ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
	broadcast_event(EventType, EntityID, EventContents, Timestamp).

%% -------------------------------------------------------------------

-spec broadcast_event(EventType, EntityID, EventContents, Timestamp) -> 'ok' when
	EventType :: pre_channel_entity:entity_event_type(),
	EntityID :: #entity_id{},
	EventContents :: [{atom(), term()}],
	Timestamp :: float().

broadcast_event(EventType, EntityID, EventContents, Timestamp) ->
	gen_server:cast(?MODULE, {entity_event, EventType, EntityID, Timestamp, EventContents}).

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

%% @hidden
init(supervisor_start) ->
	ChildSpec = {
		?MODULE,
		{pre_channel_entity, start_link, []},
        transient,
		brutal_kill,
		worker,
		[pre_channel_entity]
	},
	{ok, {{simple_one_for_one, 2, 2}, [ChildSpec]}};

init(Options) ->
	{ok, Supervisor} = supervisor:start_link({local, pre_channel_entity}, ?MODULE, supervisor_start),

	% Start worker processes.
	InitialWorkers = proplists:get_value(workers, Options, 1),
	WorkerPids = [begin
		{ok, Pid} = supervisor:start_child(Supervisor, []),
		Pid
	end || _ <- lists:seq(1, InitialWorkers)],

	?debug("Registering client hooks."),
	pre_hooks:add_hook(client_disconnected, ?MODULE, client_disconnect_hook, undefined, [node()]),
	pre_hooks:add_hook(client_inhabited_entity, ?MODULE, client_inhabited_entity_hook, undefined, [node()]),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_logged_in_hook, undefined, [node()]),

	?info("Starting fake entity event timer."),
	Timer = timer:apply_interval(4000, ?MODULE, fake_update, [#entity_id{engine = self(), ref = make_ref()}]),
	?info("Timer started: ~p", [Timer]),

	State = #state{supervisor_pid = Supervisor, worker_pids = WorkerPids},
	{ok, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_cast({client_logged_in, ClientInfo}, State) ->
	ClientCount = State#state.client_count + 1,
	[FirstWorker | OtherWorkers] = State#state.worker_pids,
	pre_channel_entity:client_connected(FirstWorker, ClientInfo),
	NewState = State#state{
		worker_pids = OtherWorkers ++ [FirstWorker],
		client_count = ClientCount
	},
	{noreply, NewState};

handle_cast({client_disconnected, ClientPid, Reason}, State) ->
	ClientCount = State#state.client_count - 1,
	State1 = broadcast_to_workers(client_disconnected, [ClientPid, Reason], State),
    {noreply, State1#state{client_count = ClientCount}};

handle_cast({client_inhabited_entity, ClientPid, EntityID}, State) ->
	State1 = broadcast_to_workers(client_inhabited_entity, [ClientPid, EntityID], State),
    {noreply, State1};

handle_cast({entity_event, Type, EntityID, Timestamp, EventContents}, State) ->
	Content = pre_channel_entity:build_state_event(Type, EventContents, EntityID, Timestamp),
	State1 = broadcast_to_workers(broadcast_event, [EntityID, Content], State),
	{noreply, State1};

handle_cast(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_info({entity_event, Type, EntityID, Timestamp, EventContents}, State) ->
	handle_cast({entity_event, Type, EntityID, Timestamp, EventContents}, State);

handle_info(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

%% @hidden
terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

%% @hidden
code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% -------------------------------------------------------------------
%% Internal
%% -------------------------------------------------------------------

%% @doc Handle a client login hook call.
client_logged_in_hook(undefined, ClientInfo) ->
	?debug("Client ~p logged in; sending it to a worker process.", [ClientInfo]),
	gen_server:cast(?MODULE, {client_logged_in, ClientInfo}),
	%timer:apply_after(6000, ?MODULE, fake_update, [list_to_binary(erlang:ref_to_list(make_ref()))]),
	{ok, undefined}.

%% -------------------------------------------------------------------

%% @doc Handle a client inhabited entity hook call.
client_inhabited_entity_hook(undefined, ClientPid, EntityID) ->
	?debug("Client ~p inhabited entity ~p; notifying worker process.", [ClientPid, EntityID]),
	gen_server:cast(?MODULE, {client_inhabited_entity, ClientPid, EntityID}),
	{ok, undefined}.

%% -------------------------------------------------------------------

%% @doc Handle a client disconnect hook call.
client_disconnect_hook(undefined, ClientPid, Reason) ->
	?debug("Client process ~p disconnected for reason ~p; notifying worker processes.", [ClientPid, Reason]),
	gen_server:cast(?MODULE, {client_disconnected, ClientPid, Reason}),
	{ok, undefined}.

%% -------------------------------------------------------------------

broadcast_to_workers(Func, Args, State) ->
	[apply(pre_channel_entity, Func, [Worker | Args])
		|| Worker <- State#state.worker_pids],
	State.

%% -------------------------------------------------------------------

%% @hidden
fake_update(EntityID) ->
	?info("Sending fake update for entity ~p.", [EntityID]),
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
	?MODULE ! {entity_event, full, EntityID, Timestamp, [
		{modelDef, {struct, [
			{model, <<"Ships/ares">>}
		]}},
		{timestamp, Timestamp},
		{state, {struct, [
			% Flying in a circle.
			{position, [100, 500, -10]},
			{position_vel, [0, 30, 0]},
			{position_acc_abs, [0, 0, 0]},
			{position_acc_rel, [-9, 0, 0]},
			{orientation, [1, 0, 0, 0]},
			{orientation_vel, [0.9887710779360422, 0.0, 0.0, 0.14943813247359922]},
			{orientation_acc_abs, [1, 0, 0, 0]},
			{orientation_acc_rel, [1, 0, 0, 0]},

			% Listing lazily to the left!
			%{position, [0, 200, -10]},
			%{position_vel, [0, 20, 0]},
			%{position_acc_abs, [0, 0, 0]},
			%{position_acc_rel, [0, 1, 3]},
			%{orientation, [1, 0, 0, 0]},
			%{orientation_vel, [1, 0, 0, 0]},
			%{orientation_acc_abs, [1, 0, 0, 0]},
			%{orientation_acc_rel, [0.9988960616987121, 0.01743579561349186, -0.043612743921365014, -0.0007612632768451531]},

			{behavior, <<"Physical">>}
		]}}
	]}.
