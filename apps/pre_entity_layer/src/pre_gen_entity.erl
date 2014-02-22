%%% @doc Behavior for entities to use when added to a gen_event manager,
%%% and associated functions to fire events into a gen_event manager so
%%% a callback module can be used.
%%% 
%%% The callback module detemines if a given entity_event is valuable or
%%% not. It is expected that most events will simple end up being a no-op.
%%%
%%% The state of an entity is persisted, and can be recovered.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_gen_entity).

-callback init(Args :: list(any())) -> {'ok', any()}.
-callback handle_info(Name :: atom(), From :: 'undefined' | any(), To :: 'undefined' | any(), State :: any()) -> {'ok', any()}.
-callback removed(Why :: any(), State :: any()) -> any().
-callback stopping(State :: any()) -> 'persist' | {'persist', any()} | 'ok'.

-type event_mgr_ref() :: pid() | atom() | {atom(), node()} | {'global', atom()} | {'via', atom(), atom()}.
-opaque timeout_msg() :: {'$pre_gen_entity_timer', any(), any(), any(), any()}.
-opaque notify_after_timer() :: {timeout_msg(), reference()}.

-export_type([timeout_msg/0, notify_after_timer/0]).

-behavior(gen_event).

% API
-export([
	ensure_mnesia_table/0,
	add_entity/4,
	add_sup_entity/4,
	recover_entity/3,
	recover_sup_entity/3,
	retrieve_ids_by_manager/1,
	notify/5,
	notify_after/5,
	cancel_notify/1
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
	sup, module, id, state
}).

-record(?MODULE, {
	% the '_' make it easy to build a matchspec.
	id = '_', state = '_', gen_event = '_' 
}).

%% ---------------------------------------------------------------------------------------------------------------------
%% External API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Ensures that the mnesia table exists. When put into a supervisor
%% tree, the supervisor should call this function before any gen_event
%% managers have pre_gen_enities added to them.
-spec ensure_mnesia_table() -> 'ok' | any().

ensure_mnesia_table() ->
	CreateArgs = [
		{local_content, true},
		{attributes, record_info(fields, ?MODULE)}
	],
	case mnesia:create_table(?MODULE, CreateArgs) of
		{atomic, ok} -> ok;
		{aborted, {already_exists, ?MODULE}} -> ok;
		{aborted, Else} -> Else
	end.

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

%% @doc If a given entity is persistend, attempt to recover it and
%% continue on under the contorl of the give gen_event manager
%% `GenEventRef'. Attempts to recover the same entity to multiple gen_event
%% managers are undefiend.
-spec recover_entity(GenEventRef :: event_mgr_ref(), EntityId :: any(), CallbackMod :: atom()) -> ok | {error, not_found}.

recover_entity(GenEventRef, EntityId, CallbackMod) ->
	recover_entity(GenEventRef, EntityId, CallbackMod, undefined).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc If a given entity is persistend, attempt to recover it and
%% continue on under the contorl of the give gen_event manager
%% `GenEventRef' supervised by the given process `SupPid'. This is a
%% combination of {@link recover_entity/3} and {@link add_sup_entity/4}.
%% Attempts to recover the same entity to multiple gen_event
%% managers are undefiend.
%% @see add_sup_entity/4
%% @see recover_entity/3
-spec recover_entity(GenEventRef :: event_mgr_ref(), EntityId :: any(), CallbackMod :: atom(), SupPid :: pid()) -> ok | {error, not_found}.

recover_entity(GenEventRef, EntityId, CallbackMod, SupPid) ->
  case mnesia:dirty_read(?MODULE, {CallbackMod, EntityId}) of
    [State] ->
      gen_event:add_handler(GenEventRef, {?MODULE, {CallbackMod, EntityId}}, {recover, SupPid, State});
    [] ->
      {error, not_found}
  end.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Recover an entity supervisored by the calling process.
%% @see recover_entitiy/4
-spec recover_sup_entity(GenEventRef :: event_mgr_ref(), EntityId :: any(), CallbackMod :: atom()) -> ok.

recover_sup_entity(GenEventRef, EntityId, CallbackMod) ->
	recover_entity(GenEventRef, EntityId, CallbackMod, self()).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Return the list of entity id's the given event_event manager
%% `GenEventPid' is running. The Id in this case is a tuple of the callback
%% module and the id passed into the recover or add functions.
-spec retrieve_ids_by_manager(GenEventPid :: event_mgr_ref()) -> [{atom(), any()}].

retrieve_ids_by_manager(GenEventPid) ->
	Recs = mnesia:dirty_match_object(#?MODULE{gen_event = GenEventPid}),
	[Id || #?MODULE{id = Id} <- Recs].

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

%% ---------------------------------------------------------------------------------------------------------------------
%% gen_event callbacks
%% ---------------------------------------------------------------------------------------------------------------------

%% @private
init({recover, SupPid, StoredRec}) ->
	#?MODULE{id = {Module, EntityId}, state = SubState} = StoredRec,
	State = #state{sup = SupPid, module = Module, id = EntityId, state = SubState},
	backend_store({Module, EntityId}, SubState),
	{ok, State};

init({EntityId, SupPid, Module, Args}) ->
	case Module:init(Args) of
		{ok, SubState} ->
			backend_store({Module, EntityId}, SubState),
			State = #state{sup = SupPid, module = Module, id = EntityId, state = SubState},
			{ok, State};
		Else ->
			Else
	end.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_event({'$pre_gen_entity_event', EventName, FromId, ToId, Data}, State) ->
	#state{module = Module, state = SubState} = State,
	case Module:handle_event(EventName, FromId, ToId, Data, SubState) of
		{ok, NewSubState} ->
			backend_store({Module, State#state.id}, NewSubState),
			{ok, State#state{state = NewSubState}};
		remove_entity ->
			remove_handler
	end;

handle_event(Event, State) ->
	lager:info("Some unhandled event: ~p", [Event]),
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_call(_,State) -> {ok, {error, invalide}, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_info({'$pre_gen_entity_timer', EventName, FromId, ToId, Data},State) ->
	#state{module = Module, state = SubState} = State,
	case Module:handle_event(EventName, FromId, ToId, Data, SubState) of
		{ok, NewSubState} ->
			backend_store({Module, State#state.id}, NewSubState),
			{ok, State#state{state = NewSubState}};
		remove_entity ->
			remove_handler
	end;

handle_info(_, State) ->
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
terminate(remove_handler, State) ->
	#state{module = Module, id = Id, state = SubState} = State,
	_ = Module:removed(remove_entity, SubState),
	ok = mnesia:dirty_delete(?MODULE, {Module, Id}),
	case State#state.sup of
		undefined ->
			ok;
		Pid when is_pid(Pid) ->
			Pid ! {pre_gen_entity, entity_removed, self(), {Module, Id}}
	end;

terminate(stop, State) ->
	#state{module = Module, state = SubState} = State,
	case Module:stopping(SubState) of
		{persist, NewSubState} ->
			backend_store({Module, State#state.id}, NewSubState);
		persist ->
			ok;
		_ ->
			mnesia:dirty_delete(?MODULE, {Module, State#state.id})
	end;

terminate(_Why, _State) ->
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
code_change(_,_,State) -> {ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------------------------------

backend_store(Key, Val) ->
	Rec = #?MODULE{ id = Key, state = Val, gen_event = self()},
	mnesia:dirty_write(Rec).

