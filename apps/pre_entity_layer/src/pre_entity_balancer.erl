-module(pre_entity_balancer).

-behavior(gen_server).

-include_lib("stdlib/include/qlc.hrl").

% api
-export([start_link/0, start_link/1, stats/0,
	add_entity/3, recover_entity/2, notify/4]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

% api

start_link() ->
	start_link([]).

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stats() ->
	qlc:e(qlc:q([Row || {Pid, _} = Row <- ets:table(?MODULE), is_pid(Pid)])).

add_entity(Module, Id, Args) ->
	gen_server:call(?MODULE, {add_entity, Module, Id, Args}, infinity).

recover_entity(Module, Id) ->
	gen_server:call(?MODULE, {recover_entity, Module, Id}, infinity).
	
notify(EventName, From, To, Data) ->
	Pids = pg2:get_members(?MODULE),
	[pre_gen_entity:notify(Pid, EventName, From, To, Data) || Pid <- Pids].

% gen_server

init(Options) ->
	ets:new(?MODULE, [named_table, public]),
	pg2:create(?MODULE),
	State = resync([]),
	{ok, State}.

handle_call({add_entity, Module, Id, Args}, _From, State) ->
	GenEvent = least_loaded(),
	Reply = maybe_add_sup_entity(GenEvent, Id, Module, Args),
	maybe_update_stats(Reply, GenEvent),
	State2 = maybe_resync(Reply, State),
	{reply, Reply, State2};

handle_call({recover_entity, Module, Id}, _From, State) ->
	GenEvent = least_loaded(),
	Reply = maybe_recover_sup_entity(GenEvent, Id, Module),
	maybe_update_stats(Reply, GenEvent),
	State2 = maybe_resync(Reply, State),
	{reply, Reply, State2};

handle_call(_Req, _From, State) ->
	{reply, {error, invalid}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({pre_gen_entity, entity_removed, GenEvent, _}, State) ->
	ets:update_counter(?MODULE, GenEvent, {2, -1}),
	{noreply, State};

handle_info({'DOWN', Mon, process, Pid, _Why}, State) ->
	State2 = remove_worker(Mon, State),
	ets:delete(?MODULE, Pid),
	schedule_resync(),
	{noreply, State2};

handle_info(resync, State) ->
	{noreply, resync(State)};

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Why, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% internal

schedule_resync() ->
	erlang:send_after(100, self(), resync).

maybe_resync(ok, State) ->
	State;

maybe_resync(_Else, State) ->
	resync(State).

resync(KnownWorkers) ->
	Kids = pre_ge_sup:running_children(),
	NewWorkers = lists:foldl(fun(Kid, Acc) ->
		case lists:keyfind(Kid, 2, KnownWorkers) of
			false ->
				Mon = erlang:monitor(process, Kid),
				ets:insert(?MODULE, {Kid, 0}),
				pg2:join(?MODULE, Kid),
				orddict:store(Mon, Kid, Acc);
			{Mon, Kid} ->
				orddict:store(Mon, Kid, Acc)
		end
	end, [], Kids),
	lists:foreach(fun({_Mon, Pid}) ->
		case lists:member(Pid, Kids) of
			false -> ets:delete(?MODULE, Pid);
			_ -> ok
		end
	end, KnownWorkers),
	NewWorkers.

maybe_add_sup_entity(GenEvent, Id, Module, Args) ->
	try pre_gen_entity:add_sup_entity(GenEvent, Id, Module, Args) of
		ok -> ok
	catch
		'EXIT':noproc ->
			schedule_resync(),
			{error, event_manager_exit}
	end.

maybe_recover_sup_entity(GenEvent, Id, Module) ->
	try pre_gen_entity:add_sup_entity(GenEvent, Id, Module) of
		ok -> ok
	catch
		'EXIT':noproc ->
			schedule_resync(),
			{error, event_manager_exit}
	end.

least_loaded() ->
	Loads = stats(),
	{Pid, _} = lists:foldl(fun lighter_load/2, {undefined, undefined}, Loads),
	Pid.

lighter_load({Pid, Load} = New, {_OtherPid, HeavierLoad}) when Load < HeavierLoad ->
	New;
lighter_load(_Nope, Current) ->
	Current.

maybe_update_stats(ok, GenEvent) ->
	ets:update_counter(?MODULE, GenEvent, {2, 1});

maybe_update_stats(_, _) ->
	ok.

remove_worker(Mon, State) ->
	orddict:erase(Mon, State).

