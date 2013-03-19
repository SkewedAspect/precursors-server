%%% @doc The entity simulation engine.
%%%
%%% This module is designed to be the heart of our entity simulation engine. It holds on to a dictionary of entities,
%%% and every simulation interval it runs through all of it's entities, and calls `simulate` on their behaviors. The only
%%% other thing it does is to provide behaviors a simple way to inform any watchers that something has changed in the
%%% entity's state. It's not in charge of maintaining that list; it's just the one who sends the messages.
%%%
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_entity_engine).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([start_link/1]).
-export([add_entity/2, remove_entity/2, get_entity/2, update_entity_state/4]).
-export([add_watcher/3, remove_watcher/3, send_to_watchers/2]).
-export([client_request/6, client_event/5]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Simulation interval
-define(INTERVAL, 17). % 1/60th of a second.

% Full Update interval
-define(FULL_INTERVAL, 30000). % 30 seconds.

-record(state, {
	entities :: dict()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% --------------------------------------------------------------------------------------------------------------------
%% Enitty API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Adds the given entity to our simulation list.
%%
%% This assumes we are being passed a valid entity record, and adds it to our simulation list.
-spec add_entity(Pid::pid(), Entity::#entity{}) ->
	ok | {error, Msg::list()}.

add_entity(Pid, Entity) ->
	gen_server:call(Pid, {add, Entity}).

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
%% Watcher API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Adds a watcher to be notified on updates.
%%
%% Adds a watcher pid to the list of watchers for this entity. This always assumes the watcher was correctly added.
-spec add_watcher(Pid::pid(), EntityID::binary(), Watcher::pid()) ->
	ok.

add_watcher(Pid, EntityID, Watcher) ->
	gen_server:cast(Pid, {add_watcher, EntityID, Watcher}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Removes a watcher from the entity..
%%
%% Removes a watcher pid from the entity. This always assumes the watcher was correctly removed.
-spec remove_watcher(Pid::pid(), EntityID::binary(), Watcher::pid()) ->
	ok.

remove_watcher(Pid, EntityID, Watcher) ->
	gen_server:cast(Pid, {remove_watcher, EntityID, Watcher}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sends an update message to watchers for the given entity.
%%
%% This sends an `{updated, EntityID, State}` message to all pids in the entity's watchers list. This does not report
%% errors if it fails to send the message.
-spec send_to_watchers(Entity::#entity{}, State::json()) ->
	ok.

send_to_watchers(Entity, State) ->
	Watchers = Entity#entity.watchers,
	send_to_watchers(Watchers, Entity#entity.id, State).

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
		entities = dict:new()
	},

	% Start the simulation timer
	erlang:send_after(?INTERVAL, self(), simulate),

	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({add, Entity}, _From, State) ->
	Entities = State#state.entities,
	NewState = State#state {
		entities = dict:append(Entity#entity.id, Entity, Entities)
	},

	% Start full update timer
	erlang:send_after(?FULL_INTERVAL, self(), {full_update, Entity#entity.id}),
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


handle_call({update, EntityID, _OldEntState, _NewEntState}, _From, State) ->
	Entities = State#state.entities,
	%TODO: Compare OldEntState to the current entity's state. If the match, we then update to NewEntState. Otherwise, we
	% error.
	NewState = State#state {
		entities = lists:store(EntityID, #entity{}, Entities)
	},
    {reply, ok, NewState};


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

handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(simulate, State) ->
	% Simulate all our entities
	simulate_entities(dict:to_list(State#state.entities), State),

	% Start new timer
    erlang:send_after(?INTERVAL, self(), simulate),

    {noreply, State};


handle_info({full_update, EntityID}, State) ->
	Entities = State#state.entities,
	_Entity = dict:fetch(EntityID, Entities),

	%TODO: Send full update here!

	% Reset timer
	erlang:send_after(?FULL_INTERVAL, self(), {full_update, EntityID}),

    {noreply, State};


handle_info({add_watcher, EntityID, Watcher}, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	Watchers = Entity#entity.watchers,

	% Append new watcher
	NewWatchers = lists:append(Watchers, [Watcher]),

	% Update Entity
	NewEntity = Entity#entity {
		watchers = NewWatchers
	},

	% Update state
	NewState = State#state {
		entities = dict:store(EntityID, NewEntity, Entities)
	},

    {noreply, NewState};

handle_info({remove_watcher, EntityID, Watcher}, State) ->
	Entities = State#state.entities,
	Entity = dict:fetch(EntityID, Entities),
	Watchers = Entity#entity.watchers,

	% Remove Watcher
	NewWatchers = lists:delete(Watcher, Watchers),

	% Update Entity
	NewEntity = Entity#entity {
		watchers = NewWatchers
	},

	% Update state
	NewState = State#state {
		entities = dict:store(EntityID, NewEntity, Entities)
	},

    {noreply, NewState};


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

simulate_entities([], _State) ->
	ok;

simulate_entities([{EntityID, Entity} | Rest], State) ->
	% Simulate this entity's behavior.
	Behavior = Entity#entity.behavior,
	NewEntity = apply(Behavior, simulate, [{Entity, State}]),
	dict:store(EntityID, NewEntity, State#state.entities),
	simulate_entities(Rest, State).


send_to_watchers([], _EntityID, _State) ->
	ok;

send_to_watchers([Watcher | Rest], EntityID, State) ->
	% Send `{update, EntityID, State}` to Watcher
	Watcher ! {update, EntityID, State},
	send_to_watchers(Rest, EntityID, State).

%% --------------------------------------------------------------------------------------------------------------------
