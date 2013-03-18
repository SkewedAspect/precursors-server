%%% @doc The entity simulation engine.

-module(pre_entity_engine).
-behavior(gen_server).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([start_link/1]).
-export([add_entity/1, remove_entity/1, update_entity_state/3]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	entities :: dict()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Looks up the given entity by id, and then adds it to our simulation list.
%%
%% This attempts to look up the entity from pre_data, and then adds it to the simulation list.
-spec add_entity(EntityID::binary()) ->
	ok | {error, Msg::list()}.

add_entity(EntityID) ->
	gen_server:call({add, EntityID}).

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
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([]) ->
	State = #state{
		entities = dict:new()
	},
	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({add, EntityID}, _From, State) ->
	Entities = State#state.entities,
	%TODO: look up the entity in pre_data, and replace `some_val`
	NewState = State#state {
		entities = dict:append(EntityID, some_val, Entities)
	},
    {reply, ok, NewState};

handle_call({remove, EntityID}, _From, State) ->
	Entities = State#state.entities,
	NewState = State#state {
		%TODO: Handle exception if EntityID is not found.
		entities = dict:erase(EntityID, Entities)
	},
    {reply, ok, NewState};

handle_call({update, EntityID, _OldEntState, _NewEntState}, _From, State) ->
	Entities = State#state.entities,
	%TODO: Compare OldEntState to the current entity's state. If the match, we then update to NewEntState. Otherwise, we
	% error.
	NewState = State#state {
		entities = lists:delete(EntityID, some_val, Entities)
	},
    {reply, ok, NewState};

handle_call(_, _From, State) ->
    {reply, invalid, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.
