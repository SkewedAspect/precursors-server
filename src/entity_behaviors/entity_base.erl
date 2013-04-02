%%% @doc An base behavior. Mostly useful as something to call into to from other behaviors.
%%% -------------------------------------------------------------------------------------------------------------------
-module(entity_base).

-include("log.hrl").
-include("pre_entity.hrl").
-include("pre_physics.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/2, simulate/2, get_full_state/1, client_request/6, client_event/5]).

% helpers
-export([diff_state/2, calc_update/2]).

-define(STEP_SIZE, 50).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, Behavior) ->
	State = dict:new(),
	#entity{
		id = EntityID,
		behavior = Behavior,
		state = State
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(Entity, _EntityEngineState) ->
	{undefined, Entity}.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(EntityState) ->
	{[{behavior, <<"Base">>}], EntityState}.

%% --------------------------------------------------------------------------------------------------------------------

client_request(EntityState, _ClientInfo, Channel, RequestType, _RequestID, Request) ->
	?debug("~p received invalid request ~p on channel ~p! (full request: ~p)",
		[EntityState#entity.id, RequestType, Channel, Request]),
	Response = {reply, [
		{confirm, false},
		{reason, <<"Base entity does not acknowledge your pathetic requests.">>}
	]},
	{Response, EntityState}.

%% --------------------------------------------------------------------------------------------------------------------

client_event(EntityState, _ClientInfo, Channel, EventType, Event) ->
	?debug("~p received invalid event ~p on channel ~p! (full event: ~p)",
		[EntityState#entity.id, EventType, Channel, Event]),
	{noreply, EntityState}.

%% --------------------------------------------------------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Returns the difference of two state dictionaries.
%%
%% Assumes that both states are dictionaries, returns a list of tuples of Key, Value.

-spec diff_state(OldState :: dict(), NewState :: dict()) ->
	[{Key :: binary(), Value::term()}].

diff_state(OldState, NewState) ->
	dict:fold(fun(Key, Value, AccIn) ->
		NewVal = dict:fetch(Key, NewState),
		case Value == NewVal of
			false ->
				[{Key, NewVal} | AccIn];
			_ ->
				AccIn
		end
	end, [], OldState).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Calculates the update record, and returns an appropriate update tuple.
%%
%% This assumes that Entity has not been updated already, therefore, it performs the update, and then calculates the diff
%% between the states, and returns a properly formated return tuple.

calc_update(NewState, Entity) ->
	OldState = Entity#entity.state,
	case diff_state(OldState, NewState) of
		[] ->
			{undefined, Entity};
		Update ->
			NewEntity = Entity#entity{
				state = NewState
			},
			{Update, NewEntity}
	end.

