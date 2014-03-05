%%% @doc The base entity controller. Mostly useful as something to call into to from other controllers.
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_base).

-behaviour(entity_controller).

% API
-export([diff_state/2]).

% entity_controller
-export([init/3, simulate/2, get_full_state/2, client_request/6, client_event/5, entity_event/4, apply_update/4]).

-record(state, {
	model_def :: list(),

	id :: term(),
	controller :: module()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% entity_controller
%% --------------------------------------------------------------------------------------------------------------------

init(EntityID, Controller, InitData) ->
	#state{
		model_def = proplists:get_value(model_def, InitData),

		id = EntityID,
		controller = Controller
	}.

%% --------------------------------------------------------------------------------------------------------------------

simulate(_Controller, State) ->
	{[], State}.

%% --------------------------------------------------------------------------------------------------------------------

get_full_state(_Controller, State) ->
	ModelDef = State#state.model_def,

	{<<"Base">>, [
		{base, [{modelDef, ModelDef}]}
	]}.

%% --------------------------------------------------------------------------------------------------------------------

client_request(entity, <<"full">>, _RequestID, _Request, Controller, State) ->
	StateUpdate = [
		{confirm, true}
		| Controller:get_full_state(State)
	],

	Response = pre_channel_entity:build_state_event(undefined, StateUpdate, State#state.id),
	{Response, [], State};

client_request(Channel, RequestType, _RequestID, Request, Controller, State) ->
	lager:debug("~p received unhandled request ~p on channel ~p! (full request: ~p)",
		[State#state.id, RequestType, Channel, Request]),

	% Respond humorously.
	ControllerBin = atom_to_binary(Controller, latin1),
	Response = [
		{confirm, false},
		{reason, <<ControllerBin/binary, " entity does not acknowledge your pathetic requests.">>}
	],

	{Response, [], State}.

%% --------------------------------------------------------------------------------------------------------------------

client_event(Channel, EventType, Event, _Controller, State) ->
	lager:debug("~p received unhandled event ~p on channel ~p! (full event: ~p)",
		[State#state.id, EventType, Channel, Event]),

	{[], State}.

%% --------------------------------------------------------------------------------------------------------------------

entity_event(Event, From, _Controller, State) ->
	lager:debug("~p received unhandled entity event from ~p! (full event: ~p)",
		[State#state.id, From, Event]),

	{[], State}.

%% --------------------------------------------------------------------------------------------------------------------

apply_update(base, [], _Controller, State) ->
	{[], State};

apply_update(base, [{modelDef, NewValue} | Rest], Controller, State) ->
	State1 = State#state {
		model_def = NewValue
	},
	apply_update(base, Rest, Controller, State1);

apply_update(base, [{SubKey, NewValue} | Rest], Controller, State) ->
	lager:warning("Ignoring unrecognized update for subkey ~p of base! (value: ~p)", [SubKey, NewValue]),
	apply_update(base, Rest, Controller, State);

apply_update(Key, SubKeys, _Controller, State) ->
	lager:warning("Ignoring unrecognized update key ~p! (subkeys: ~p)", [Key, SubKeys]),
	{[], State}.

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Returns the difference of two state proplists.
%%
%% Assumes that both states are proplists; returns a filtered version of the new state, only containing items that have
%% changed.

-spec diff_state(OldState, NewState) -> Update when
	OldState :: StateMessage,
	NewState :: StateMessage,
	Update :: StateMessage,
	StateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: term().

diff_state(OldState, NewState) ->
	lists:filter(
		fun({Key, NewVal}) ->
			case proplists:get_value(Key, OldState) of
				NewVal -> false;
				_ -> true
			end
		end,
		NewState
	).
