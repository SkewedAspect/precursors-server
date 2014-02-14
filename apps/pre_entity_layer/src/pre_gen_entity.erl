-module(pre_gen_entity).

-callback init(Args :: list(any())) -> {'ok', any()}.
-callback handle_info(Name :: atom(), From :: 'undefined' | any(), To :: 'undefined' | any(), State :: any()) -> {'ok', any()}.

-behavior(gen_event).

% api
-export([
	ensure_mnesia_table/0,
	add_entity/4,
	add_sup_entity/4,
	recover_entity/3,
	recover_sup_entity/3,
	retrieve_ids_by_manager/1,
	notify/5
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

% pulic api

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

add_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, {CallbackMod, EntityId}}, {EntityId, undefined, CallbackMod, Args}).

add_sup_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, {CallbackMod, EntityId}}, {EntityId, self(), CallbackMod, Args}).

recover_entity(GenEventRef, EntityId, CallbackMod) ->
	recover_entity(GenEventRef, EntityId, CallbackMod, undefined).

recover_sup_entity(GenEventRef, EntityId, CallbackMod) ->
	recover_entity(GenEventRef, EntityId, CallbackMod, self()).

recover_entity(GenEventRef, EntityId, CallbackMod, SupPid) ->
	case mnesia:dirty_read(?MODULE, {CallbackMod, EntityId}) of
		[State] ->
			gen_event:add_handler(GenEventRef, {?MODULE, {CallbackMod, EntityId}}, {recover, SupPid, State});
		[] ->
			{error, not_found}
	end.

retrieve_ids_by_manager(GenEventPid) ->
	Recs = mnesia:dirty_match_object(#?MODULE{gen_event = GenEventPid}),
	[Id || #?MODULE{id = Id} <- Recs].

notify(GenEventRef, EventName, FromId, ToId, Data) ->
	gen_event:notify(GenEventRef, {'$pre_gen_entity_event', EventName, FromId, ToId, Data}).

% gen_event callbacks

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

handle_call(_,_) -> ok.

handle_info(_,_) -> ok.

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

code_change(_,_,_) -> ok.

%% internal functions

backend_store(Key, Val) ->
	Rec = #?MODULE{ id = Key, state = Val, gen_event = self()},
	mnesia:dirty_write(Rec).

