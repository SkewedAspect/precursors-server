%%% @doc An base behavior. Mostly useful as something to call into to from other behaviors.
%%% -------------------------------------------------------------------------------------------------------------------
-module(entity_base).

-include("log.hrl").
-include("pre_entity.hrl").

-behaviour(entity_behavior).

% pre_entity
-export([init/1, simulate/2, get_client_behavior/0, get_full_state/1, client_request/5, client_event/5,
	entity_event/3]).

% helpers
-export([gen_full_state/3, gen_full_state/2, gen_full_state/1, diff_state/2, calc_update/2]).

-define(STEP_SIZE, 50).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

init(Entity) ->
	Entity#entity{
		state = dict:new()
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(Entity, _EntityEngineState) ->
	{undefined, Entity}.

%% --------------------------------------------------------------------------------------------------------------------

get_client_behavior() ->
	<<"Base">>.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(Entity) ->
	ModelDef = dict:fetch(modelDef, Entity#entity.state),

	[{modelDef, ModelDef}].

%% --------------------------------------------------------------------------------------------------------------------

client_request(Entity, entity, <<"full">>, _RequestID, _Request) ->
	Behavior = Entity#entity.behavior,
	StateUpdate = [
		{confirm, true},
		{state, Behavior:get_full_state(Entity)}
	],

	Response = pre_channel_entity:build_state_event(undefined, StateUpdate, Entity#entity.id),
	{Response, undefined, Entity};

client_request(Entity, Channel, RequestType, _RequestID, Request) ->
	?debug("~p received unhandled request ~p on channel ~p! (full request: ~p)",
		[Entity#entity.id, RequestType, Channel, Request]),

	% Respond humorously.
	BehaviorBin = atom_to_binary(Entity#entity.behavior, latin1),
	Response = [
		{confirm, false},
		{reason, <<BehaviorBin/binary, " entity does not acknowledge your pathetic requests.">>}
	],

	{Response, undefined, Entity}.

%% --------------------------------------------------------------------------------------------------------------------

client_event(Entity, _ClientInfo, Channel, EventType, Event) ->
	?debug("~p received unhandled event ~p on channel ~p! (full event: ~p)",
		[Entity#entity.id, EventType, Channel, Event]),

	{undefined, Entity}.

%% --------------------------------------------------------------------------------------------------------------------

entity_event(Event, From, Entity) ->
	?debug("~p received unhandled entity event from ~p! (full event: ~p)",
		[Entity#entity.id, From, Event]),

	{undefined, Entity}.

%% --------------------------------------------------------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Returns a json term representing the full state of the entity.
%%
%% Allows you to pass in a transformation function and initial accumulator state for the generation. The transformation
%% function is expected to take a value, and return the new value.

-spec gen_full_state(TransformFun :: fun(), InitialAcc :: list(), State :: dict()) ->
	[{Key :: binary(), Value::term()}].

gen_full_state(TransformFun, InitialAcc, State) ->
	dict:fold(fun(Key, Value, AccIn) ->
		NewVal = TransformFun(Value),
		[{Key, NewVal} | AccIn]
	end, InitialAcc, State).


%% @doc Returns a json term representing the full state of the entity.
%%
%% Allows you to pass in a transformation function _or_ initial accumulator state for the generation. If the
%% transformation function it omitted, it simply assumes value requires no transformation.

-spec gen_full_state(TransformFun | InitialAcc, State :: dict()) ->
	[{Key :: binary(), Value::term()}] when
	TransformFun :: fun(),
	InitialAcc :: list().


gen_full_state(TransformFun, State) when is_function(TransformFun) ->
	dict:fold(fun(Key, Value, AccIn) ->
		NewVal = TransformFun(Value),
		[{Key, NewVal} | AccIn]
		end, [], State);

gen_full_state(InitialAcc, State) when is_list(InitialAcc) ->
	dict:fold(fun(Key, Value, AccIn) ->
		[{Key, Value} | AccIn]
		end, InitialAcc, State).


%% @doc Returns a json term representing the full state of the entity.
%%
%% Assumes value requires no transformation, and simply returns the json.

-spec gen_full_state(State :: dict()) ->
	[{Key :: binary(), Value::term()}].


gen_full_state(State) ->
	dict:fold(fun(Key, Value, AccIn) ->
		[{Key, Value} | AccIn]
		end, [], State).

%% --------------------------------------------------------------------------------------------------------------------


%% @doc Returns the difference of two state dictionaries.
%%
%% Assumes that both states are dictionaries, returns a list of tuples of Key, Value.

-spec diff_state(OldState :: dict(), NewState :: dict()) ->
	[{Key :: binary(), Value::term()}].

diff_state(OldState, NewState) ->
	dict:fold(fun(Key, OldValue, AccIn) ->
		NewValue = dict:fetch(Key, NewState),
		case OldValue == NewValue of
			false ->
				case Key of
					physical ->
						%[{Key, pre_physics_rk4:diff_to_proplist(OldValue, NewValue)} | AccIn];
						[{Key, pre_physics_rk4:to_proplist(NewValue)} | AccIn];
					_ ->
						[{Key, NewValue} | AccIn]
				end;
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
