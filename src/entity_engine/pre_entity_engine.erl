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
-include("pre_entity_new.hrl").

% API
-export([start_link/1]).
-export([add_entity/1, remove_entity/1, update_entity_state/3]).
-export([add_watcher/2, remove_watcher/2, send_to_watchers/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Simulation interval
-define(INTERVAL, 17). % 1/60th of a second.

-record(state, {
	entities :: dict()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Adds the given entity to our simulation list.
%%
%% This assumes we are being passed a valid entity record, and adds it to our simulation list.
-spec add_entity(Entity::#entity{}) ->
	ok | {error, Msg::list()}.

add_entity(Entity) ->
	gen_server:call({add, Entity}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Removes the entity given by id from the simulation list.
%%
%% Looks up the entity
-spec remove_entity(EntityID::binary()) ->
	ok | not_found | {error, Msg::list()}.

remove_entity(EntityID) ->
	gen_server:call({remove, EntityID}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Looks up the given entity by id, and then adds it to our simulation list.
%%
%% This attempts to look up the entity from pre_data, and then adds it to the simulation list.
-spec update_entity_state(EntityID::binary(), OldState::json(), NewState::json()) ->
	ok | newer_version | {error, Msg::list()}.

update_entity_state(EntityID, OldState, NewState) ->
	gen_server:call({update, EntityID, OldState, NewState}).

%% --------------------------------------------------------------------------------------------------------------------
%% Watcher API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Adds a watcher to be notified on updates.
%%
%% Adds a watcher pid to the list of watchers for this entity. This always assumes the watcher was correctly added.
-spec add_watcher(EntityID::binary(), Watcher::pid()) ->
	ok.

add_watcher(EntityID, Watcher) ->
	gen_server:cast({add_watcher, EntityID, Watcher}).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Removes a watcher from the entity..
%%
%% Removes a watcher pid from the entity. This always assumes the watcher was correctly removed.
-spec remove_watcher(EntityID::binary(), Watcher::pid()) ->
	ok.

remove_watcher(EntityID, Watcher) ->
	gen_server:cast({remove_watcher, EntityID, Watcher}).

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
    {reply, ok, NewState};

handle_call({remove, EntityID}, _From, State) ->
	Entities = State#state.entities,
	NewState = State#state {
		entities = dict:erase(EntityID, Entities)
	},
    {reply, ok, NewState};

handle_call({update, EntityID, _OldEntState, _NewEntState}, _From, State) ->
	Entities = State#state.entities,
	%TODO: Compare OldEntState to the current entity's state. If the match, we then update to NewEntState. Otherwise, we
	% error.
	NewState = State#state {
		entities = lists:store(EntityID, #entity{}, Entities)
	},
    {reply, ok, NewState};

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
