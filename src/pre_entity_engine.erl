%%% @doc The entity engine worker - calculates the state of a collection of entities and forwards events to a given entity's logic callback module.

-module(pre_entity_engine).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([get_owning_client/1, get_full_state/1, get_full_state_async/2]).
-export([client_request/5, create_entity/2]).

% gen_server
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	entities :: term()
}).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% -------------------------------------------------------------------

%% @doc Get the given entity's current state.
-spec get_full_state(Entity) -> [{atom(), term()}] when
	Entity :: #entity{} | #entity_id{}.
get_full_state(#entity{id = EntityID}) ->
	get_full_state(EntityID);

get_full_state(#entity_id{} = EntityID) ->
	#entity_id{engine = Pid} = EntityID,
	gen_server:call(Pid, {get_full_state, EntityID}).

%% -------------------------------------------------------------------

-spec get_full_state_async(Entity, Function) -> Return when
	Entity :: #entity{},
	Function :: {module(), atom()} | {module(), atom(), list()} | fun((Timestamp, FullState) -> Return),
	Timestamp :: float(),
	FullState :: [{atom(), term()}],
	Return :: term().

%% @doc Get the given entity's current state, then call the given function with that state as its (first) argument.

get_full_state_async(Entity, {Mod, Func}) ->
	get_full_state_async(Entity, {Mod, Func, []});

get_full_state_async(Entity, {Mod, Func, Args}) ->
	spawn(fun () ->
		begin
			{Timestamp, FullState} = get_full_state(Entity),
			apply(Mod, Func, [Timestamp, FullState | Args])
		end
	end);

get_full_state_async(Entity, Fun) ->
	spawn(fun () ->
		begin
			{Timestamp, FullState} = get_full_state(Entity),
			Fun(Timestamp, FullState)
		end
	end).

%% -------------------------------------------------------------------

get_owning_client(Entity) ->
	Entity#entity.client.

%% -------------------------------------------------------------------

client_request(EntityID, From, Channel, RequestID, Request) ->
	#entity_id{engine = Pid} = EntityID,
	RequestType = request_type(Request),
	gen_server:cast(Pid, {client_request, EntityID, From, Channel, RequestType, RequestID, Request}),
	ok.

%% -------------------------------------------------------------------

create_entity(Pid, Behavior) ->
	gen_server:call(Pid, {create_entity, Behavior}).

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

init([]) ->
	EntityID = #entity_id{
		engine = self(),
		ref = make_ref()
	},
	Entity = #entity{id = EntityID},
	Entities = dict:store(EntityID, Entity, dict:new()),
	State = #state{entities = Entities},
	{ok, State}.

%% -------------------------------------------------------------------

handle_call({get_full_state, #entity_id{} = EntityID}, _From, State) ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,

	{FullState, State1} = call_entity_func(get_full_state, EntityID, [], State),
	{ClientBehavior, State2} = call_entity_func(get_client_behavior, EntityID, [], State1),

	{reply, {Timestamp, [{behavior, ClientBehavior} | FullState]}, State2};

handle_call({create_entity, Behavior}, _From, State) ->
	EntityID = #entity_id{
		engine = self(),
		ref = make_ref()
	},
	Entity = #entity{
		id = EntityID,
		callback_module = Behavior
	},
	Entities = dict:store(EntityID, Entity, State#state.entities),
	State1 = #state{entities = Entities},
    {reply, EntityID, State1};

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

handle_cast({client_request, EntityID, From, Channel, RequestType, RequestID, Request}, State) ->
	Result = client_request_internal(EntityID, Channel, RequestType, RequestID, Request, State),
	case Result of
		{ok, Response, State1} ->
			FullMessage = {struct, Response},
			pre_client_connection:send(From, tcp, {response, RequestID}, entity, FullMessage),
			{noreply, State1};
		{noreply, State2} ->
			?warning("Got ~p in response to a client_request! This should always return a response.", [Result]),
			{noreply, State2}
	end;

handle_cast(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

handle_info({timer_fired, EntityID, Args}, State) ->
	Result = call_entity_func(timer_fired, EntityID, Args, State),
	case Result of
		{ok, _Response, State1} ->
			{noreply, State1};
		{noreply, State2} ->
			{noreply, State2}
	end;

handle_info(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

%% -------------------------------------------------------------------

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% -------------------------------------------------------------------

client_request_internal(EntityID, <<"entity">>, <<"full">>, _RequestID, _Request, State) ->
	#state{entities = Entities} = State,
	Entity = dict:fetch(EntityID, Entities),
	#entity{
		model_def = ModelDef
	} = Entity,
	{Timestamp, FullState} = pre_entity_engine:get_full_state(Entity),
	Response = [
		{result, ok},
		{id, EntityID},
		{timestamp, Timestamp},
		{modelDef, {struct, ModelDef}},
		{state, {struct, FullState}}
	],
	{ok, Response, State};

client_request_internal(EntityID, Channel, RequestType, RequestID, Request, State) ->
	call_entity_func(client_request, EntityID, [Channel, RequestType, RequestID, Request], State).

%% -------------------------------------------------------------------

request_type({struct, Request}) ->
	proplists:get_value(<<"type">>, Request);

request_type(_) ->
	undefined.

%% -------------------------------------------------------------------

call_entity_func(Func, EntityID, Args, State) ->
	#state{entities = Entities} = State,
	Entity = dict:fetch(EntityID, Entities),
	CallbackModule = Entity#entity.callback_module,
	Result = apply(CallbackModule, Func, [Entity | Args]),
	case Result of
		{Result1, Result2, Entity2} ->
			Entities1 = dict:store(EntityID, Entity2, Entities),
			{Result1, Result2, State#state{entities = Entities1}};
		{Result3, Entity3} ->
			Entities2 = dict:store(EntityID, Entity3, Entities),
			{Result3, State#state{entities = Entities2}}
	end.
