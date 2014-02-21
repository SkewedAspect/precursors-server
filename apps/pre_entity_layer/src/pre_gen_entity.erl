%%% @doc A `gen_event' that comprises the heart of the Entity Event Engine.
%%%
%%% Handles incoming events, determines which entity's interesting in the event, and then calls the correct callback
%%% module for the entity.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_gen_entity).

-callback init(Args :: list(any())) -> {'ok', any()}.
-callback handle_info(Name :: atom(), From :: 'undefined' | any(), To :: 'undefined' | any(), State :: any()) -> {'ok', any()}.
-callback removed(Why :: any(), State :: any()) -> any().
-callback stopping(State :: any()) -> 'persist' | {'persist', any()} | 'ok'.

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

%% @doc Ensures that the mnesia table exists. (Used for unit tests.)
-spec ensure_mnesia_table() -> ok | any().

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

%% @doc Adds an entity to this `pre_gen_event'.
-spec add_entity(GenEventPid :: pid(), EntityId :: any(), CallbackMod :: atom() | { atom(), term() }, Args :: list()) -> ok.

add_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, {CallbackMod, EntityId}}, {EntityId, undefined, CallbackMod, Args}).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Add a supervised entity to this `pre_gen_event'.
-spec add_sup_entity(GenEventPid :: pid(), EntityId :: any(), CallbackMod :: atom() | { atom(), term() }, Args :: list()) -> ok.

add_sup_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, {CallbackMod, EntityId}}, {EntityId, self(), CallbackMod, Args}).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Attempts to recover from an error, by reloading the entity from mnesia.
-spec recover_entity(GenEventRef :: pid() | any(), EntityId :: any(), CallbackMod :: atom() | { atom(), term() }) -> ok | {error, not_found}.

recover_entity(GenEventRef, EntityId, CallbackMod) ->
	recover_entity(GenEventRef, EntityId, CallbackMod, undefined).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Attempts to recover from an error, by reloading the entity from mnesia.
-spec recover_entity(GenEventRef :: pid() | any(), EntityId :: any(), CallbackMod :: atom() | { atom(), term() }, SupPid :: pid()) -> ok | {error, not_found}.

recover_entity(GenEventRef, EntityId, CallbackMod, SupPid) ->
  case mnesia:dirty_read(?MODULE, {CallbackMod, EntityId}) of
    [State] ->
      gen_event:add_handler(GenEventRef, {?MODULE, {CallbackMod, EntityId}}, {recover, SupPid, State});
    [] ->
      {error, not_found}
  end.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc
-spec recover_sup_entity(GenEventRef :: pid() | any(), EntityId :: any(), CallbackMod :: atom() | { atom(), term() }) -> ok.

recover_sup_entity(GenEventRef, EntityId, CallbackMod) ->
	recover_entity(GenEventRef, EntityId, CallbackMod, self()).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc
-spec retrieve_ids_by_manager(GenEventPid :: pid()) -> ok.

retrieve_ids_by_manager(GenEventPid) ->
	Recs = mnesia:dirty_match_object(#?MODULE{gen_event = GenEventPid}),
	[Id || #?MODULE{id = Id} <- Recs].

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc
-spec notify(GenEventRef :: pid() | any(), EventName :: term(), FromId :: any(), ToId :: any(), Data :: any()) -> ok.

notify(GenEventRef, EventName, FromId, ToId, Data) ->
	gen_event:notify(GenEventRef, {'$pre_gen_entity_event', EventName, FromId, ToId, Data}).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc
-spec notify_after(Time :: integer(), EventName :: term(), FromId :: any(), ToId :: any(), Data :: any()) -> { Msg :: tuple(), Tref :: reference() }.

notify_after(Time, EventName, FromId, ToId, Data) ->
	Msg = {'$pre_gen_entity_timer', EventName, FromId, ToId, Data},
	Tref = erlang:send_after(Time, self(), Msg),
	{Msg, Tref}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc
-spec cancel_notify({ Msg :: tuple(), Tref :: reference() }) -> integer() | false.

cancel_notify({Msg, Tref}) ->
	Out = erlang:cancel_timer(Tref),
	receive Msg -> ok after 0 -> ok end,
	Out.

%% ---------------------------------------------------------------------------------------------------------------------
%% gen_event callbacks
%% ---------------------------------------------------------------------------------------------------------------------

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

handle_call(_,_) -> ok.

%% ---------------------------------------------------------------------------------------------------------------------

handle_info({'$pre_gen_entity_timer', EventName, FromId, ToId, Data},State) ->
	#state{module = Module, state = SubState} = State,
	case Module:handle_event(EventName, FromId, ToId, Data, SubState) of
		{ok, NewSubState} ->
			backend_store({Module, State#state.id}, NewSubState),
			{ok, State#state{state = NewSubState}};
		remove_entity ->
			remove_handler
	end.

%% ---------------------------------------------------------------------------------------------------------------------

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

code_change(_,_,_) -> ok.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------------------------------

backend_store(Key, Val) ->
	Rec = #?MODULE{ id = Key, state = Val, gen_event = self()},
	mnesia:dirty_write(Rec).

