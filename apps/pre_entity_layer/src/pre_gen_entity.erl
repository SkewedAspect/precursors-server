%%% @doc Behavior for entities to use when added to a gen_event manager,
%%% and associated functions to fire events into a gen_event manager so
%%% a callback module can be used.
%%%
%%% The callback module detemines if a given entity_event is valuable or
%%% not. It is expected that most events will simple end up being a no-op.
%%%
%%% The state of an entity is persisted, and can be recovered.

-module(pre_gen_entity).

-callback init(Args :: list(any())) -> {'ok', any()}.
-callback simulate(Delta :: integer(), State :: any()) -> {'ok', any()}.
-callback handle_event(Name :: atom(), From :: 'undefined' | any(), To :: 'undefined' | any(), Data :: any(), State :: any()) -> {'ok', any()} | 'remove_entity'.
-callback removed(Why :: any(), State :: any()) -> any().
-callback stopping(State :: any()) -> 'persist' | {'persist', any()} | 'ok'.

-type event_mgr_ref() :: pid() | atom() | {atom(), node()} | {'global', atom()} | {'via', atom(), atom()}.
-opaque timeout_msg() :: {'$pre_gen_entity_timer', any(), any(), any(), any()}.
-opaque notify_after_timer() :: {timeout_msg(), reference()}.

-export_type([timeout_msg/0, notify_after_timer/0]).

-behavior(gen_event).

-ifndef(SIMULATE_INTERVAL).
-define(SIMULATE_INTERVAL, 33). % about 1/30 of a second
-endif.

% API
-export([
	add_entity/4,
	add_sup_entity/4,
	retrieve_ids_by_manager/1,
	notify/5,
	notify_after/5,
	cancel_notify/1,
	run_simulations/1,
	simulate_interval/0
]).

% gen_event
-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	sup, module, id, target_sim_time, state
}).

-record(?MODULE, {
	% the '_' make it easy to build a matchspec.
	id = '_', state = '_', gen_event = '_'
}).

%% ---------------------------------------------------------------------------------------------------------------------
%% External API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Adds a pre_gen_entity to the given gen_event_manager `GenEventPid'.
-spec add_entity(GenEventPid :: event_mgr_ref(), EntityId :: any(), CallbackMod :: atom(), Args :: [any()]) -> 'ok'.

add_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, {CallbackMod, EntityId}}, {EntityId, undefined, CallbackMod, Args}).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Add a supervised entity to the given gen_event_manager
%% `GenEventPid'. If the entitiy is removed (for any reason), a message
%% is sent to the calling process:
%%
%%     `{pre_gen_entity, entity_removed, GenEventPid, {CallbackModule, Id}}'
%%
%% This DOES NOT automatically monitor the gen_event manager.
-spec add_sup_entity(GenEventPid :: event_mgr_ref(), EntityId :: any(), CallbackMod :: atom(), Args :: [any()]) -> 'ok'.

add_sup_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, {CallbackMod, EntityId}}, {EntityId, self(), CallbackMod, Args}).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Return the list of entity id's the given event_event manager
%% `GenEventPid' is running. The Id in this case is a tuple of the callback
%% module and the id passed into the recover or add functions.
-spec retrieve_ids_by_manager(GenEventPid :: event_mgr_ref()) -> [{atom(), any()}].

retrieve_ids_by_manager(GenEventPid) ->
	Ids = gen_event:which_handlers(GenEventPid),
	[Id || {?MODULE, Id} <- Ids].

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Send a `pre_gen_entity' event to the given gen_event manager
%% `GenEventRef'. `Any pre_gen_entity' handlers that have been added to the
%% event manager will receive the event and call the `handle_event/4'
%% function of the callback module.
-spec notify(GenEventRef :: event_mgr_ref(), EventName :: any(), FromId :: any(), ToId :: any(), Data :: any()) -> ok.

notify(GenEventRef, EventName, FromId, ToId, Data) ->
	gen_event:notify(GenEventRef, {'$pre_gen_entity_event', EventName, FromId, ToId, Data}).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Creates a timer to send an event after `Time' milliseconds. The
%% timer can be canceled by passing the return value to
%% {@link cancel_notify/1}. Note that the timer, once expired, will send
%% a message to the process that called the function, and so is best used
%% in a pre_gen_entity callback module; this ensures a pre_gen_entity
%% handler will get the timeout message and fire the correct event. This
%% also means if the gen_event manager is handling more than just
%% pre_gen_entity's, the other handles must be able to handle unexpected
%% messages.
%% @see cancel_notify/1
-spec notify_after(Time :: pos_integer(), EventName :: term(), FromId :: any(), ToId :: any(), Data :: any()) -> notify_after_timer().

notify_after(Time, EventName, FromId, ToId, Data) ->
	Msg = {'$pre_gen_entity_timer', EventName, FromId, ToId, Data},
	Tref = erlang:send_after(Time, self(), Msg),
	{Msg, Tref}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Cancel a timer returned by {@link notify_after/5}. This will also
%% check the message queue for the event so it will be cleared out. This
%% means it should be called by the same process that created the timer.
%% The return value is either the number of milliseconds left on the timer,
%% or `false' if the message had already been delivered, or was not a
%% timer.
%% @see notify_after/5
-spec cancel_notify(notify_after_timer()) -> non_neg_integer() | 'false'.

cancel_notify({Msg, Tref}) ->
	Out = erlang:cancel_timer(Tref),
	receive Msg -> ok after 0 -> ok end,
	Out.

%% @doc Returns the interval used for calculating simulation intervals
%% when {@link run_simulaions/0} is called.
-spec simulate_interval() -> ?SIMULATE_INTERVAL.
simulate_interval() ->
	?SIMULATE_INTERVAL.

%% @doc For each registered entity, run thier simulate callbacks. This
%% should be called at at interval at least as large as what is returned by
%% {@link simulate_interval/0}. There is no timer set up by this module,
%% something else must do the timing. It does calculate the correct time
%% deltas and may call the simulate callback multiple times to catch it up.
-spec run_simulations(GenEventRef :: event_mgr_ref()) -> 'ok'.
run_simulations(GenEventRef) ->
	gen_event:notify(GenEventRef, '$pre_gen_entity_simulate').

%% ---------------------------------------------------------------------------------------------------------------------
%% gen_event callbacks
%% ---------------------------------------------------------------------------------------------------------------------

%% @private
init({EntityId, SupPid, Module, Args}) ->
	case Module:init(Args) of
		{ok, SubState} ->
			State = #state{sup = SupPid, module = Module, id = EntityId, state = SubState},
			{ok, State};
		Else ->
			Else
	end.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_event({'$pre_gen_entity_event', EventName, FromId, ToId, Data}, State) ->
	callback_event(EventName, FromId, ToId, Data, State);

handle_event('$pre_gen_entity_simulate', #state{target_sim_time = undefined} = State) ->
	Now = os:timestamp(),
	Expected = now_to_number(Now) + micro_interval(),
	{ok, State#state{target_sim_time = Expected}};

handle_event('$pre_gen_entity_simulate', State) ->
	{Ticks, NextExpected} = get_ticks(State#state.target_sim_time),
	Interval = micro_interval(),
	callback_simulate(Ticks, Interval, NextExpected, State);

handle_event(Event, State) ->
	lager:info("Some unhandled event: ~p", [Event]),
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_call(_,State) -> {ok, {error, invalid}, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_info({'$pre_gen_entity_timer', EventName, FromId, ToId, Data},State) ->
	callback_event(EventName, FromId, ToId, Data, State);

handle_info(_, State) ->
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
terminate(remove_handler, State) ->
	#state{module = Module, id = Id, state = SubState} = State,
	_ = Module:removed(remove_entity, SubState),
	case State#state.sup of
		undefined ->
			ok;
		Pid when is_pid(Pid) ->
			Pid ! {pre_gen_entity, entity_removed, self(), {Module, Id}}
	end;

terminate(stop, State) ->
	#state{module = Module, state = SubState} = State,
	_ = Module:stopping(SubState);

terminate(_Why, _State) ->
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
code_change(_,_,State) -> {ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------------------------------

callback_event(EventName, FromId, ToId, Data, State) ->
	#state{module = Module, state = SubState} = State,
	case Module:handle_event(EventName, FromId, ToId, Data, SubState) of
		{ok, NewSubState} ->
			{ok, State#state{state = NewSubState}};
		remove_entity ->
			remove_handler
	end.

callback_simulate(0, _Interval, NextExpected, State) ->
	{ok, State#state{target_sim_time = NextExpected}};

callback_simulate(Ticks, Interval, NextExpected, State) ->
	#state{module = Module, state = SubState} = State,
	case Module:simulate(Interval, SubState) of
		{ok, NewSubState} ->
			callback_simulate(Ticks - 1, Interval, NextExpected, State#state{state = NewSubState});
		remove_entity ->
			remove_handler
	end.

now_to_number({Mega, Sec, Micro}) ->
	( (Mega * 1000000) * 1000000) + (Sec * 1000000) + Micro.

get_ticks(NextExpected) ->
	NowTuple = os:timestamp(),
	Now = now_to_number(NowTuple),
	Interval = micro_interval(),
	get_ticks(Now, NextExpected, Interval, 0).

get_ticks(Now, NextExpected, _Interval, Ticks) when Now < NextExpected ->
	{Ticks, NextExpected};

get_ticks(Now, OldExpected, Interval, Ticks) ->
	NextExpected = OldExpected + Interval,
	get_ticks(Now, NextExpected, Interval, Ticks + 1).

micro_interval() ->
	simulate_interval() * 1000.

