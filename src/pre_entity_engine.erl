%%% @doc The entity engine worker - calculates the state of a collection of entities and forwards events to a given entity's logic callback module.

-module(pre_entity_engine).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(entity_id_internal, {
	internal_id :: term(),
	callback_module :: atom()
}).

-record(state, {
	entities :: term()
}).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

init([]) ->
	EntityID = #entity_id_internal{
		internal_id = make_ref(),
		callback_module = entity_test
	},
	Entity = #entity{id = EntityID},
	Entities = dict:store(EntityID, Entity, dict:new()),
	State = #state{entities = Entities},
	{ok, State}.

%% -------------------------------------------------------------------

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

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% -------------------------------------------------------------------

call_entity_func(Func, EntityID, Args, State) ->
	#state{entities = Entities} = State,
	Entity = dict:fetch(EntityID, Entities),
	CallbackModule = EntityID#entity_id_internal.callback_module,
	Result = apply(CallbackModule, Func, [Entity | Args]),
	case Result of
		{Status, Response, Entity2} ->
			Entities1 = dict:store(EntityID, Entity2, Entities),
			{Status, Response, State#state{entities = Entities1}};
		{Status2, Entity3} ->
			Entities2 = dict:store(EntityID, Entity3, Entities),
			{Status2, State#state{entities = Entities2}}
	end.
