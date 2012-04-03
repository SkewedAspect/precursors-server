%%% @doc The entity channel - forwards updates from nearby entity to the client.
%%%
%%% This module is the supervisor for pre_channel_entity; only one of these should exist per node.

-module(pre_channel_entity_sup).
-behavior(gen_server).

-include("log.hrl").
-include("pre_client.hrl").

% Because this saves us _so_ much code.
-define(CHANNEL, <<"entity">>).

% gen_server
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([client_connect_hook/2, client_disconnect_hook/3]).

-record(state, {
	supervisor_pid :: pid(),
	worker_pids :: [pid()],
	client_count = 0 :: non_neg_integer()
}).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

-type(workers_option() :: {'workers', non_neg_integer()}).
-type(start_option() :: workers_option()).
-type(start_options() :: [start_option()] | 'supervisor_start').

%% @doc Starts the entity channel master server.
-spec(start_link/1 :: (Options :: start_options()) -> {'ok', pid()}).
start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

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

	?debug("Starting fake entity event timer."),
	Timer = timer:send_interval(2000, {entity_event, [
		{id, list_to_binary(erlang:ref_to_list(make_ref()))},
		{modelDef, {struct, [
			{model, <<"Ships/ares">>}
		]}},
		{state, {struct, [
			{position, [0, 0, 100]}
		]}}
	]}),
	?info("Timer started: ~p", [Timer]),

	State = #state{supervisor_pid = Supervisor, worker_pids = WorkerPids},
	{ok, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_cast({client_connected, _ClientInfo} = Message, State) ->
	ClientCount = State#state.client_count + 1,
	[FirstWorker | OtherWorkers] = State#state.worker_pids,
	gen_server:cast(FirstWorker, Message),
	NewState = State#state{
		worker_pids = OtherWorkers ++ [FirstWorker],
		client_count = ClientCount
	},
	{noreply, NewState};

handle_cast({client_disconnected, _ClientPid, _Reason} = Message, State) ->
	ClientCount = State#state.client_count - 1,
	State1 = broadcast_event(Message, State),
    {noreply, State1#state{client_count = ClientCount}};

handle_cast(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

%% @hidden
handle_info({entity_event, _Content} = Message, State) ->
	State1 = broadcast_event(Message, State),
	{noreply, State1};

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

broadcast_event(Message, State) ->
	[gen_server:cast(Worker, Message)
		|| Worker <- State#state.worker_pids],
	State.
