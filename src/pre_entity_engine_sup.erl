%%% @doc The entity engine - manages entity processes and forwards events to a given entity's logic callback module.

-module(pre_entity_engine_sup).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% pre_client_channels
-export([client_request/4]).

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

client_request({Pid, EntityID}, RequestType, RequestID, Request) ->
	gen_server:cast(Pid, {client_request, EntityID, RequestType, RequestID, Request}),
	ok.

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

init(supervisor_start) ->
	ChildSpec = {
		?MODULE,
		{pre_entity_engine, start_link, []},
        transient,
		brutal_kill,
		worker,
		[pre_entity_engine]
	},
	{ok, {{simple_one_for_one, 2, 2}, [ChildSpec]}};

init(Options) ->
	InitialWorkers = proplists:get_value(workers, Options, 1),
	{ok, Supervisor} = supervisor:start_link({local, pre_entity_sup}, ?MODULE, supervisor_start),
	WorkerPids = [begin
		{ok, Pid} = supervisor:start_child(Supervisor, []),
		Pid
	end || _ <- lists:seq(1, InitialWorkers)],
	State = #state{supervisor_pid = Supervisor, worker_pids = WorkerPids},
	{ok, State}.

%% -------------------------------------------------------------------

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

handle_cast(client_connected, State) ->
	ClientCount = State#state.client_count + 1,
    {noreply, State#state{client_count = ClientCount}};

handle_cast(client_disconnected, State) ->
	ClientCount = State#state.client_count - 1,
    {noreply, State#state{client_count = ClientCount}};

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
