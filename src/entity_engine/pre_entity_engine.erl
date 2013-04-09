%%% @doc The entity simulation engine.
%%%
%%% This module is designed to be the heart of our entity simulation engine. It holds on to a dictionary of entities,
%%% and every simulation interval it runs through all of it's entities, and calls `simulate` on their behaviors. The
%%% only other thing it does is to provide behaviors a simple way to inform any watchers that something has changed in
%%% the entity's state.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_engine).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([start_link/0]).
-export([add_entity/2, receive_entity/2, remove_entity/2, get_entity/2]).
-export([client_request/6, client_event/5]).

% Internal
-export([send_entity_to/3]).

% gen_server
-ifdef(TEST).
-export([simulate_entities/1]).
-endif.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Simulation interval
-define(INTERVAL, 16). % about 1/60th of a second. (16 ms)

% Full Update interval
-define(FULL_INTERVAL, 30000). % 30 seconds.

-record(state, {
	entities = dict:new()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% Supervision API
%% --------------------------------------------------------------------------------------------------------------------

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------
%% Entity API
%% --------------------------------------------------------------------------------------------------------------------

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Adds the given entity to our simulation list.
%%
%% This assumes we are being passed a valid entity record, and adds it to our simulation list.

-spec add_entity(Pid::pid(), Entity::#entity{}) ->
	ok | {error, Msg::list()}.

add_entity(Pid, Entity) ->
	gen_server:call(Pid, {add, Entity}).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Moves the given entity to our simulation list.
%%
%% This assumes we are being passed a valid entity record, and adds it to our simulation list.

-spec receive_entity(Pid::pid(), Entity::#entity{}) ->
	ok | {error, Msg::list()}.

receive_entity(Pid, Entity) ->
	gen_server:call(Pid, {receive_entity, Entity}).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Removes the entity given by id from the simulation list.
%%
%% Removed the given entity from the simulation list.

-spec remove_entity(Pid::pid(), EntityID::binary()) ->
	ok | not_found | {error, Msg::list()}.

remove_entity(Pid, EntityID) ->
	gen_server:call(Pid, {remove, EntityID}).

%% --------------------------------------------------------------------------------------------------------------------
%% @doc Retrieves an entity by id.
%%
%% Looks up the entity by id and returns it.

-spec get_entity(Pid::pid(), EntityID::binary()) ->
	{ok, Entity::#entity{}} | not_found | {error, Msg::list()}.

get_entity(Pid, EntityID) ->
	gen_server:call(Pid, {get, EntityID}).

%% --------------------------------------------------------------------------------------------------------------------
%% Client API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Passes the client request to the appropriate behavior to be handled.
%%
%% This passes a request from the client to the behavior of the entity the client is currently controlling. A response
%% is always expected.
-spec client_request(Pid::pid(), EntityID::binary(), Channel::atom(), RequestType::binary(), RequestID::integer(), Request::json()) ->
	Response::json().

client_request(Pid, EntityID, Channel, RequestType, RequestID, Request) ->
	gen_server:call(Pid, {request, EntityID, Channel, RequestType, RequestID, Request}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Passes the client event to the appropriate behavior to be handled.
%%
%% This passes an event from the client to the behavior of the entity the client is currently controlling. No response
%% is expected.
-spec client_event(Pid::pid(), EntityID::binary(), Channel::atom(), EventType::binary(), Event::json()) ->
	Response::json().

client_event(Pid, EntityID, Channel, EventType, Event) ->
	gen_server:cast(Pid, {client_event, EntityID, Channel, EventType, Event}).


%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([]) ->
	% Join the entity_engines process group.
	pg2:create(entity_engines),
	pg2:join(entity_engines, self()),

	% Join the entity_updates process group.
	pg2:create(entity_updates),
	pg2:join(entity_updates, self()),

	% Start the simulation timer
	erlang:send_after(?INTERVAL, self(), simulate),

	{ok, #state{}}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({add, Entity}, _From, State) ->
	NewState = add_entity_internal(Entity, State),
	% Notify all engine supervisors that we now own this entity.
	pre_entity_engine_sup:cast_all({entity_added, Entity#entity.id, self()}),

	case Entity#entity.client of
		undefined -> ok;
		ClientInfo ->
			% Notify this client's connection.
			pre_client_connection:set_inhabited_entity(ClientInfo#client_info.connection, Entity, self())
	end,
    {reply, ok, NewState};


handle_call({remove, EntityID}, _From, State) ->
	Entities = State#state.entities,
	NewState = State#state {
		entities = dict:erase(EntityID, Entities)
	},
    {reply, ok, NewState};


handle_call({get, EntityID}, _From, State) ->
	Entities = State#state.entities,
    case dict:find(EntityID, Entities) of
        {ok, Entity} ->
            {reply, {ok, Entity}, State};
        error ->
            {reply, {error, not_found}, State}
    end;


handle_call({receive_entity, Entity}, _From, State) ->
	NewState = add_entity_internal(Entity, State),
	% FromEngine should be calling all engine supervisors with {entity_moved, ...}.
    {reply, ok, NewState};

handle_call({request, EntityID, Channel, RequestType, RequestID, Request}, _From, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	call_behavior_func(Entity, client_request, [Entity, Channel, RequestType, RequestID, Request], State);


handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast({client_event, EntityID, Channel, EventType, Event}, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	call_behavior_func(Entity, client_event, [Entity, Channel, EventType, Event], State);

handle_cast({send_entity, EntityID, TargetNode}, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	spawn(?MODULE, send_entity_to, [self(), Entity, TargetNode]),
    {noreply, State};

handle_cast({update, EntityID, Update}, State) ->
	dict:fold(fun(TargetEntityID, TargetEntity, _Acc) ->
		case {TargetEntityID, TargetEntity#entity.client} of
			{EntityID, _} -> ok;
			{_, undefined} -> ok;
			{_, ClientInfo} ->
				%TODO: Filter according to distance from TargetEntity or something.
				pre_entity_comm:send_update(ClientInfo, EntityID, Update)
		end
	end, ok, State#state.entities),
	{noreply, State};


handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(simulate, State) ->
	% Simulate all our entities
	State1 = simulate_entities(State),

	% Start new timer
    erlang:send_after(?INTERVAL, self(), simulate),

    {noreply, State1};


handle_info(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal API
%% --------------------------------------------------------------------------------------------------------------------

simulate_entities(State) ->
	Entities = State#state.entities,
	NewEntities = dict:map(fun(_EntityID, Entity) ->
		{noreply, NewEntity} = call_behavior_func(Entity, simulate, [Entity, State]),
		NewEntity
	end, Entities),
	State#state{
		entities = NewEntities
	}.


add_entity_internal(Entity, State) ->
	Entities = State#state.entities,
	EntityID = Entity#entity.id,

	State#state {
		entities = dict:store(EntityID, Entity, Entities)
	}.


send_entity_to(FromEnginePid, Entity, TargetNode) ->
	{ok, NewEnginePid} = gen_server:call({TargetNode, pre_entity_engine_sup}, {move_to_local_engine, Entity, FromEnginePid}),
	ok = gen_server:call(FromEnginePid, {forward_to, Entity, NewEnginePid}),

	%TODO: Test responses!
	pre_entity_engine_sup:call_all({entity_moved, Entity#entity.id, NewEnginePid}),

	ok = gen_server:call(FromEnginePid, {forget, Entity}).


call_behavior_func(#entity{} = Entity, Func, Args, State) ->
	case call_behavior_func(Entity, Func, Args) of
		{reply, Reply, NewEntity1} ->
			{reply, Reply, update_entity_state(NewEntity1, State)};
		{noreply, NewEntity2} ->
			{noreply, update_entity_state(NewEntity2, State)}
	end;

call_behavior_func(EntityID, Func, Args, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	call_behavior_func(Entity, Func, Args, State).


call_behavior_func(Entity, Func, Args) ->
	#entity{
		id = EntityID,
		behavior = Behavior,
		client = ClientInfo
	} = Entity,

	% Call the behavior
	try apply(Behavior, Func, Args) of
		{undefined, NewEntity1} ->
			{noreply, NewEntity1};

		{Update1, NewEntity2} ->
			send_update(ClientInfo, EntityID, Update1),
			{noreply, NewEntity2};

		{Reply1, undefined, NewEntity3} ->
			{reply, Reply1, NewEntity3};

		{Reply2, Update2, NewEntity4} ->
			send_update(ClientInfo, EntityID, Update2),
			{reply, Reply2, NewEntity4}

	catch
		Exception ->
			?error("Exception while calling behavior function ~p:~p(~p) for entity ~p: ~p",
				[Behavior, Func, Args, EntityID, Exception]),
			Entity
	end.


update_entity_state(NewEntity, State) ->
	State#state{
		entities = dict:store(NewEntity#entity.id, NewEntity, State#state.entities)
	}.


send_update(undefined, EntityID, Update) ->
	% Send the entity update to all other entity engines
	pre_entity_engine_sup:broadcast_update(EntityID, Update);

send_update(ClientInfo, EntityID, Update) ->
	% Send the entity update to this entity's client
	pre_entity_comm:send_update(ClientInfo, EntityID, Update),
	send_update(undefined, EntityID, Update).
