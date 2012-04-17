%%% @doc The entity engine worker - calculates the state of a collection of entities and forwards events to a given entity's logic callback module.

-module(pre_entity_engine).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([start_link/0, get_owning_client/1, get_full_state/1, get_full_state_async/2]).
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

%% @doc Get the given entity's current state, then call the given function with that state as its (first) argument.
-spec get_full_state_async(Entity, Function) -> Return when
	Entity :: #entity{},
	Function :: {module(), atom()} | {module(), atom(), list()} | fun(([{atom(), term()}]) -> Return),
	Return :: term().
get_full_state_async(Entity, {Mod, Func}) ->
	get_full_state_async(Entity, {Mod, Func, []});

get_full_state_async(Entity, {Mod, Func, Args}) ->
	spawn(fun () ->
		apply(Mod, Func, [get_full_state(Entity) | Args])
	end);

get_full_state_async(Entity, Fun) ->
	spawn(fun () ->
		Fun(get_full_state(Entity))
	end).

%% -------------------------------------------------------------------

get_owning_client(Entity) ->
	Entity#entity.client.

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
	{ok, FullUpdate, State2} = call_entity_func(get_full_state, EntityID, [], State),
	%TODO: Possibly add common tuples here.
	{reply, FullUpdate, State2};

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% -------------------------------------------------------------------

handle_cast({client_request, EntityID, RequestType, RequestID, Request}, State) ->
	Result = call_entity_func(client_request, EntityID, [RequestType, RequestID, Request], State),
	case Result of
		{ok, _Response, State1} ->
			{noreply, State1};
		{noreply, State2} ->
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

call_entity_func(Func, EntityID, Args, State) ->
	#state{entities = Entities} = State,
	Entity = dict:fetch(EntityID, Entities),
	CallbackModule = Entity#entity.callback_module,
	Result = apply(CallbackModule, Func, [Entity | Args]),
	case Result of
		{Status, Response, Entity2} ->
			Entities1 = dict:store(EntityID, Entity2, Entities),
			{Status, Response, State#state{entities = Entities1}};
		{Status2, Entity3} ->
			Entities2 = dict:store(EntityID, Entity3, Entities),
			{Status2, State#state{entities = Entities2}}
	end.
