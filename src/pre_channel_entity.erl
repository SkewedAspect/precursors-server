%%% @doc Dictates the background content loaded by the client, and dispatches events for entities in a given zone. (?)
%%% FIXME: This module should not actually be used! It's a useful example of how to build a channel, though.

-module(pre_channel_entity).
-behavior(gen_server).

-include("log.hrl").

% gen_server
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% pre_client_channels
-export([client_request/3, client_response/4, client_event/3]).

-record(state, {
		supervisor_pid :: pid(),
		worker_pids :: [pid()],
		client_count = 0 :: non_neg_integer()
	}).

%% -------------------------------------------------------------------
%% api
%% -------------------------------------------------------------------

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% -------------------------------------------------------------------

client_request(Client, Request, Pid) ->
	gen_server:cast(Pid, {client_request, Client, Request}),
	ok.

client_response(_Client, _Id, _Response, _Pid) ->
	%gen_server:cast(Pid, {client_response, Client, Id, Response}),
	ok.

client_event(_Client, _Event, _Pid) ->
	%gen_server:cast(Pid, {client_event, Client, Event}),
	ok.

%% -------------------------------------------------------------------

client_connect_hook(undefined, _ClientPid) ->
	gen_server:cast(?MODULE, client_connected).

client_disconnect_hook(undefined, _ClientPid) ->
	gen_server:cast(?MODULE, client_disconnected).

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

init(supervisor_start) ->
	ChildSpec = {
		?MODULE,
		{pre_channel_entity_worker, start_link, []},
        transient,
		brutal_kill,
		worker,
		[pre_channel_entity_worker]
	},
	{ok, {{simple_one_for_one, 2, 2}, [ChildSpec]}};

init(Options) ->
	InitialWorkers = proplists:get_value(workers, Options, 1),
	{ok, Supervisor} = supervisor:start_link({local, pre_channel_entity_sup}, ?MODULE, supervisor_start),
	WorkerPids = [begin
		{ok, Pid} = supervisor:start_child(Supervisor, []),
		Pid
	end || _ <- lists:seq(1, InitialWorkers)],
	pre_hooks:add_hook(client_connected, ?MODULE, client_connect_hook, undefined, [node()]),
	pre_hooks:add_hook(client_disconnected, ?MODULE, client_disconnect_hook, undefined, [node()]),
	State = #state{supervisor_pid = Supervisor, worker_pids = WorkerPids},
	{ok, State}.

%% -------------------------------------------------------------------

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

handle_cast({client_request, _Client, _Request} = Message, State) ->
	[FirstWorker | OtherWorkers] = State#state.worker_pids,
	gen_server:cast(FirstWorker, Message),
	{noreply, State#state{worker_pids = OtherWorkers ++ [FirstWorker]}};

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
