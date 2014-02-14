-module(pre_entity_balancer).

-behavior(gen_server).

-include_lib("stdlib/include/qlc.hrl").

% api
-export([start_link/0, start_link/1, stats/0,
	add_entity/3, notify/4]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	workers = []
}).

% api

start_link() ->
	start_link([]).

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stats() ->
	qlc:e(qlc:q([Row || {Pid, _} = Row <- ets:table(?MODULE), is_pid(Pid)])).

add_entity(Module, Id, Args) ->
	Loads = stats(),
	{Pid, _} = lists:foldl(fun lighter_load/2, {undefined, undefined}, Loads),
	ok = gen_server:call(?MODULE, {add_entity, Pid, Module, Id, Args}, infinity),
	ets:update_counter(?MODULE, Pid, {2, 1}),
	ok.

lighter_load({Pid, Load} = New, {_OtherPid, HeavierLoad}) when Load < HeavierLoad ->
	New;
lighter_load(_Nope, Current) ->
	Current.

notify(EventName, From, To, Data) ->
	Pids = pg2:get_members(?MODULE),
	[pre_gen_entity:notify(Pid, EventName, From, To, Data) || Pid <- Pids].

% gen_server

init(Options) ->
	ets:new(?MODULE, [named_table, public]),
	pg2:create(?MODULE),
	NumWorkers = proplists:get_value(workers, Options, 5),
	WorkerMon = lists:foldl(fun(_N, WorkerAcc) ->
		start_worker(WorkerAcc)
	end, [], lists:seq(1, NumWorkers)),
	{ok, #state{workers = WorkerMon}}.

handle_call({add_entity, GenEvent, Module, Id, Args}, _From, State) ->
	Reply = pre_gen_entity:add_sup_entity(GenEvent, Id, Module, Args),
	{reply, Reply, State};

handle_call(_Req, _From, State) ->
	{reply, {error, invalid}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({pre_gen_entity, entity_removed, GenEvent, _}, State) ->
	ets:update_counter(?MODULE, GenEvent, {2, -1}),
	{noreply, State};

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Why, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% internal

start_worker(MonDict) ->
	{ok, Pid} = pre_ge_sup:start_child(),
	ets:insert(?MODULE, {Pid, 0}),
	Mon = erlang:monitor(process, Pid),
	pg2:join(?MODULE, Pid),
	orddict:store(Mon, Pid, MonDict).

