%%% @doc Load balances `pre_gen_entity' assignment among gen_event managers.
%%% Uses {@link pre_ge_sup} to start and supervisor the managers. This will
%%% supervisor the entities and recover them if they fail. It will also
%%% monitor the gen_event managers and attempt to move the entities it was
%%% handling to a new one.
%%%
%%% The load strategy is to always add an entity to the gen_event manager
%%% with the fewest entities assigned to it. If there is a tie, then one
%%% is chosen arbitrarily.
%%%
%%% Also allows sending the same event to all gen_event managers.
%%% --------------------------------------------------------------------------------------------------------------------

% The way this works out, it will always end up doing a round-robin in the
% event entities are added, but not removed. That's because entities
% determine when they are removed, so no balancing is done there.
% However, it is smart enough to always add to the least-loaded
% gen_event manager. In the future, this could be made smart enough to
% spawn more gen_event managers, or tear down unloaded ones.

-module(pre_entity_balancer).

-behavior(gen_server).

-include_lib("stdlib/include/qlc.hrl").

% API
-export([start_link/0, start_link/1, stats/0,
	add_entity/3, recover_entity/2, notify/3, notify/4]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% ---------------------------------------------------------------------------------------------------------------------
%% External API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Same as `start_link([]).'
%% @see start_link/1
-spec start_link() -> {'ok', pid()}.

start_link() ->
	start_link([]).

%% @doc Start the load balancer supervisored by the calling process with
%% the given options.
%% Currently, there are no options, so just give us an empty list, please.
-spec start_link([]) -> {'ok', pid()}.

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Returns a list of the gen_event managers that are used for
%% balancing and the number of entities currently on each one.
-spec stats() -> [ {pid(), non_neg_integer()} ].

stats() ->
	qlc:e(qlc:q([Row || {Pid, _} = Row <- ets:table(?MODULE), is_pid(Pid)])).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Adds an entity to the least loaded gen_event manager.
-spec add_entity(Module :: atom(), Id :: any(), Args :: [any()]) -> 'ok' | { 'error', 'event_manager_exit' }.

add_entity(Module, Id, Args) ->
	gen_server:call(?MODULE, {add_entity, Module, Id, Args}, infinity).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Tries to recover an entity and add it to the least loaded
%% gen_event manager.
%% @see pre_gen_event:recover_entity/3
-spec recover_entity(Module :: atom(), Id :: any()) ->  'ok' | { 'error', 'event_manager_exit' }.

recover_entity(Module, Id) ->
	gen_server:call(?MODULE, {recover_entity, Module, Id}, infinity).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Sends the same event to all gen_event managers monitored.
%% @see pre_entity_balancer:notify/4
-spec notify(EventName :: any(), To :: any(), Data :: any()) -> ['ok'].

notify(EventName, To, Data) ->
	notify(EventName, undefined, To, Data).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Sends the same event to all gen_event managers monitored.
%% @see pre_gen_entity:notify/5
-spec notify(EventName :: any(), From :: any(), To :: any(), Data :: any()) -> ['ok'].

notify(EventName, From, To, Data) ->
	Pids = pg2:get_members(?MODULE),
	[pre_gen_entity:notify(Pid, EventName, From, To, Data) || Pid <- Pids].

%% ---------------------------------------------------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------------------------------------------------

%% @private
init(_Options) ->
	ets:new(?MODULE, [named_table, public]),
	pg2:create(?MODULE),
	State = resync([]),
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
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

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
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

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
terminate(_Why, _State) ->
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------------------------------

schedule_resync() ->
	erlang:send_after(100, self(), resync).

%% ---------------------------------------------------------------------------------------------------------------------

maybe_resync(ok, State) ->
	State;

maybe_resync(_Else, State) ->
	resync(State).

%% ---------------------------------------------------------------------------------------------------------------------

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

%% ---------------------------------------------------------------------------------------------------------------------

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

%% ---------------------------------------------------------------------------------------------------------------------

least_loaded() ->
	Loads = stats(),
	{Pid, _} = lists:foldl(fun lighter_load/2, {undefined, undefined}, Loads),
	Pid.

%% ---------------------------------------------------------------------------------------------------------------------

lighter_load({_Pid, Load} = New, {_OtherPid, HeavierLoad}) when Load < HeavierLoad ->
	New;

lighter_load(_Nope, Current) ->
	Current.

%% ---------------------------------------------------------------------------------------------------------------------

maybe_update_stats(ok, GenEvent) ->
	ets:update_counter(?MODULE, GenEvent, {2, 1});

maybe_update_stats(_, _) ->
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

remove_worker(Mon, State) ->
	orddict:erase(Mon, State).

