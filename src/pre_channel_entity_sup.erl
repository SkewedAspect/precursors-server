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
-export([client_connect_hook/2, client_disconnect_hook/3, client_logged_in_hook/2, fake_update/1]).

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

-spec broadcast_update(EntityID :: #entity_id{}, StateDelta :: [{atom(), term()}]) -> 'ok'.
broadcast_update(EntityID, StateDelta) ->
	Content = [
		{state, {struct, StateDelta}}
	],
	broadcast_event(update, EntityID, Content).

%% -------------------------------------------------------------------

%% @doc Broadcast a full update for the given entity to all eligible clients.
%%
%% FIXME: Construct the update in pre_entity_engine, and then send to the clients here!
-spec broadcast_full_update(Entity :: #entity{}) -> 'ok'.
broadcast_full_update(Entity) ->
	pre_entity_engine:get_full_state_async(Entity, fun (FullState) ->
		begin
			#entity{
				id = EntityID,
				model_def = ModelDef
			} = Entity,

			Content = [{modelDef, {struct, ModelDef}}, {state, {struct, FullState}}],
			broadcast_event(full, EntityID, Content)
		end
	end).

%	#physical{
%		position = Position,
%		position_vel = PositionVel,
%		position_acc_abs = PositionAccAbs,
%		position_acc_rel = PositionAccRel,
%		orientation = Orientation,
%		orientation_vel = OrientationVel,
%		orientation_acc_abs = OrientationAccAbs,
%		orientation_acc_rel = OrientationAccRel
%	} = Physical,
%
%	FullState = [
%		{position, vector:vec_to_list(Position)},
%		{position_vel, vector:vec_to_list(PositionVel)},
%		{position_acc_abs, vector:vec_to_list(PositionAccAbs)},
%		{position_acc_rel, vector:vec_to_list(PositionAccRel)},
%		{orientation, quaternion:quat_to_list(Orientation)},
%		{orientation_vel, quaternion:quat_to_list(OrientationVel)},
%		{orientation_acc_abs, quaternion:quat_to_list(OrientationAccAbs)},
%		{orientation_acc_rel, quaternion:quat_to_list(OrientationAccRel)}
%	],
%
%	Content = [{modelDef, {struct, ModelDef}}, {state, {struct, FullState}}],
%	broadcast_event(full, EntityID, Content).

%% -------------------------------------------------------------------

-spec broadcast_event(EventType, EntityID, EventContents) -> 'ok' when
	EventType :: pre_channel_entity:entity_event_type(),
	EntityID :: #entity_id{},
	EventContents :: [{atom(), term()}].
broadcast_event(EventType, EntityID, EventContents) ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,

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
	pre_hooks:add_hook(client_connected, ?MODULE, client_connect_hook, undefined, [node()]),
	pre_hooks:add_hook(client_disconnected, ?MODULE, client_disconnect_hook, undefined, [node()]),
	pre_hooks:add_hook(client_logged_in, ?MODULE, client_logged_in_hook, undefined, [node()]),

	%?debug("Starting fake entity event timer."),
	%Timer = timer:apply_interval(4000, ?MODULE, fake_update, [list_to_binary(erlang:ref_to_list(make_ref()))]),
	%?info("Timer started: ~p", [Timer]),

	State = #state{supervisor_pid = Supervisor, worker_pids = WorkerPids},
	{ok, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_cast({client_connected, ClientInfo}, State) ->
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

handle_cast({entity_event, Type, EntityID, Timestamp, EventContents}, State) ->
	Content = [{id, EntityID}, {type, Type}, {timestamp, Timestamp} | EventContents],
	State1 = broadcast_to_workers(broadcast_event, [Type, Content], State),
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

%% @doc Handle a client connect hook call.
client_connect_hook(undefined, ClientInfo) ->
	?debug("Client ~p connected; sending it to a worker process.", [ClientInfo]),
	gen_server:cast(?MODULE, {client_connected, ClientInfo}),
	{ok, undefined}.

%% -------------------------------------------------------------------

%% @doc Handle a client disconnect hook call.
client_disconnect_hook(undefined, ClientPid, Reason) ->
	?debug("Client process ~p disconnected for reason ~p; notifying worker processes.", [ClientPid, Reason]),
	gen_server:cast(?MODULE, {client_disconnected, ClientPid, Reason}),
	{ok, undefined}.

%% -------------------------------------------------------------------

%% @doc Handle a client login hook call.
client_logged_in_hook(undefined, _ClientInfo) ->
	timer:apply_after(6000, ?MODULE, fake_update, [list_to_binary(erlang:ref_to_list(make_ref()))]).

%% -------------------------------------------------------------------

broadcast_to_workers(Func, Args, State) ->
	[apply(pre_channel_entity, Func, [Worker | Args])
		|| Worker <- State#state.worker_pids],
	State.

%% -------------------------------------------------------------------

%% @hidden
fake_update(EntityID) ->
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
