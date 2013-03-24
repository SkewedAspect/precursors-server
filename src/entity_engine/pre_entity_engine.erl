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
-export([start_link/1]).
-export([add_entity/2, remove_entity/2, get_entity/2, update_entity_state/4]).
-export([client_request/6, client_event/5]).

% Internal
-export([send_entity_to/3]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Simulation interval
-define(INTERVAL, 16). % about 1/60th of a second. (16 ms)

% Full Update interval
-define(FULL_INTERVAL, 30000). % 30 seconds.

-record(state, {
	entities :: list()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% Supervision API
%% --------------------------------------------------------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% --------------------------------------------------------------------------------------------------------------------
%% Enitty API
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
%% @doc Looks up the given entity by id, and then adds it to our simulation list.
%%
%% This attempts to look up the entity from pre_data, and then adds it to the simulation list.

-spec update_entity_state(Pid::pid(), EntityID::binary(), OldState::json(), NewState::json()) ->
	ok | newer_version | {error, Msg::list()}.

update_entity_state(Pid, EntityID, OldState, NewState) ->
	gen_server:call(Pid, {update, EntityID, OldState, NewState}).

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
	gen_server:call(Pid, {event, EntityID, Channel, EventType, Event}).


%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([]) ->
	State = #state{
		entities = []
	},

	% Join the entity_engines process group.
	pg2:create(entity_engines),
	pg2:join(entity_engines, self()),

	% Join the entity_updates process group.
	pg2:create(entity_updates),
	pg2:join(entity_updates, self()),

	% Start the simulation timer
	erlang:send_after(?INTERVAL, self(), simulate),

	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({add, Entity}, _From, State) ->
	NewState = add_entity(Entity, State),
	% Notify all engine supervisors that we now own this entity.
	pre_entity_engine_sup:cast_all({entity_added, Entity#entity.id, self()}),
    {reply, ok, NewState};


handle_call({remove, EntityID}, _From, State) ->
	Entities = State#state.entities,
	NewState = State#state {
		entities = dict:erase(EntityID, Entities)
	},
    {reply, ok, NewState};


handle_call({get, EntityID}, _From, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
    {reply, {ok, Entity}, State};


handle_call({receive_entity, Entity}, _From, State) ->
	NewState = add_entity_internal(Entity, State),
	% FromEngine should be calling all engine supervisors with {entity_moved, ...}.
    {reply, ok, NewState};


handle_call({update, EntityID, _OldEntState, _NewEntState}, _From, State) ->
	Entities = State#state.entities,
	%TODO: Compare OldEntState to the current entity's state. If the match, we then update to NewEntState. Otherwise, we
	% error.
	NewState = State#state {
		entities = lists:store(EntityID, #entity{}, Entities)
	},
    {reply, ok, NewState};


% Handle full update requests ourself, since the behavior doesn't need to handle them.
handle_call({request, EntityID, <<"entity">>, <<"full">>, _RequestID, _Request}, _From, State) ->
	#state{entities = Entities} = State,
	Entity = dict:fetch(EntityID, Entities),
	ModelDef = Entity#entity.model,
	EntState = Entity#entity.state,

	Response = [
		{confirm, true},
		{id, EntityID},
		{timestamp, generate_timestamp()},
		{modelDef, ModelDef},
		{state, EntState}
	],
	{reply, Response, State};

handle_call({request, EntityID, Channel, RequestType, RequestID, Request}, _From, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	Behavior = Entity#entity.behavior,

	% Call the behavior
	{Response, NewState} = Behavior:client_request(Entity, Channel, RequestType, RequestID, Request),
    {reply, Response, NewState};


handle_call({event, EntityID, Channel, EventType, Event}, _From, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	Behavior = Entity#entity.behavior,

	% Call the behavior
	{Response, NewState} = Behavior:client_request(Entity, Channel, EventType, Event),
    {reply, Response, NewState};


handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast({send_entity, EntityID, TargetNode}, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	spawn(?MODULE, send_entity_to, [self(), Entity, TargetNode]),
    {reply, ok, State};


handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(simulate, State) ->
	% Simulate all our entities
	{ok, State1} = simulate_entities(State#state.entities, [], State),

	% Start new timer
    erlang:send_after(?INTERVAL, self(), simulate),

    {noreply, State1};


handle_info({full_update, EntityID}, State) ->
	Entities = State#state.entities,
	_Entity = dict:fetch(EntityID, Entities),

	%TODO: Send full update here!

	% Reset timer
	erlang:send_after(?FULL_INTERVAL, self(), {full_update, EntityID}),

    {noreply, State};


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

simulate_entities([], NewEntities, State) ->
	{ok, State#state{
		entities = NewEntities
	}};

simulate_entities([Entity | Rest], NewEntities, State) ->
	% Simulate this entity's behavior.
	Behavior = Entity#entity.behavior,
	NewEntity = case Behavior:simulate(Entity, State) of
		{noupdate, NewEntity1} ->
			NewEntity1;
		{update, UpdateJSON, NewEntity2} ->
			SelfPid = self(),
			[Pid ! {entity_update, Entity#entity.id, UpdateJSON}
				|| Pid <- pg2:get_members(entity_updates), Pid =/= SelfPid],
			NewEntity2
	end,
	simulate_entities(Rest, [NewEntity | NewEntities], State).


generate_timestamp() ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	MegaSecs * 1000000 + Secs + MicroSecs / 1000000.


add_entity_internal(Entity, State) ->
	Entities = State#state.entities,
	EntityID = Entity#entity.id,

	% Start full update timer
	erlang:send_after(?FULL_INTERVAL, self(), {full_update, EntityID}),

	State#state {
		entities = dict:append(EntityID, Entity, Entities)
	}.


send_entity_to(FromEnginePid, Entity, TargetNode) ->
	{ok, NewEnginePid} = gen_server:call({TargetNode, pre_entity_engine_sup}, {move_to_local_engine, Entity, FromEnginePid}),
	ok = gen_server:call(FromEnginePid, {forward_to, Entity, NewEnginePid}),

	%TODO: Test responses!
	pre_entity_engine_sup:call_all({entity_moved, Entity#entity.id, NewEnginePid}),

	ok = gen_server:call(FromEnginePid, {forget, Entity}).
