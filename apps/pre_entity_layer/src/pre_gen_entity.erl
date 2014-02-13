-module(pre_gen_entity).

-callback init(Args :: list(any())) -> {'ok', any()}.
-callback handle_info(Name :: atom(), From :: 'undefined' | any(), To :: 'undefined' | any(), State :: any()) -> {'ok', any()}.

-behavior(gen_event).

% api
-export([
	ensure_mnesia_table/0,
	add_entity/4,
	recover_entity/3,
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
	module, id, state
}).

% pulic api

ensure_mnesia_table() ->
	case mnesia:create_table(?MODULE, [{local_content, true}]) of
		{atomic, ok} -> ok;
		{aborted, {already_exists, ?MODULE}} -> ok;
		{aborted, Else} -> Else
	end.

add_entity(GenEventPid, EntityId, CallbackMod, Args) ->
	gen_event:add_handler(GenEventPid, {?MODULE, {CallbackMod, EntityId}}, {EntityId, CallbackMod, Args}).

recover_entity(GenEventRef, EntityId, CallbackMod) ->
	case mnesia:dirty_read(?MODULE, {CallbackMod, EntityId}) of
		[State] ->
			gen_event:add_handler(GenEventRef, {?MODULE, {CallbackMod, EntityId}}, {recover, State});
		[] ->
			{error, not_found}
	end.

notify(GenEventRef, EventName, FromId, ToId, Data) ->
	gen_event:notify(GenEventRef, {'$pre_gen_entity_event', EventName, FromId, ToId, Data}).

% gen_event callbacks

init({recover, StoredRec}) ->
	{?MODULE, {Module, EntityId}, SubState} = StoredRec,
	State = #state{module = Module, id = EntityId, state = SubState},
	{ok, State};

init({EntityId, Module, Args}) ->
	case Module:init(Args) of
		{ok, SubState} ->
			backend_store({Module, EntityId}, SubState),
			State = #state{module = Module, id = EntityId, state = SubState},
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
	mnesia:dirty_delete(?MODULE, {Module, Id});

terminate(_,_) -> ok.

code_change(_,_,_) -> ok.

%% internal functions

backend_store(Key, Val) ->
	Rec = {?MODULE, Key, Val},
	mnesia:dirty_write(Rec).

