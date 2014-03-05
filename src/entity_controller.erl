%%% @doc The behaviour for all of our entity controller modules.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(entity_controller).

%% --------------------------------------------------------------------------------------------------------------------
%% Behaviour
%% --------------------------------------------------------------------------------------------------------------------

%%% @doc Called when a new entity is created (or loaded from the database). It should setup the default state required
%%% by the controller. It should return an opaque state.

-callback init(EntityID, Controller, InitData) -> State when
	EntityID :: term(),
	Controller :: module(),
	InitData :: [{Key, Value}],
	Key :: atom(),
	Value :: term(),
	State :: term().

%%% -------------------------------------------------------------------------------------------------------------------

%%% @doc Called every simulation frame by the entity engine. It is expected to return `{Update, NewState}' where
%%% `Update' is either a JSON structure (a proplist) representing any changes or `[]' if no changes occurred, and
%%% `NewState' is the new entity record that should be used for future simulation.

-callback simulate(Controller, State) -> {StateUpdateMessage, NewState} when
	Controller :: module(),
	State :: term(),
	StateUpdateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: term(),
	NewState :: State.

%%% -------------------------------------------------------------------------------------------------------------------

%%% @doc Called whenever a full JSON state message is required. It is expected to return a JSON structure representing
%%% the entity's current state.

-callback get_full_state(Controller, State) -> {ClientController, StateMessage} when
	Controller :: module(),
	State :: term(),
	ClientController :: binary(),
	StateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: term().

%%% -------------------------------------------------------------------------------------------------------------------

%%% @doc Called by the entity/client communication interface (pre_entity_comm) when the client sends a request that the
%%% controller needs to process. It is expected to return `{ok, Response, NewState}' where `Response' is a JSON
%%% structure to send back to the client.

-callback client_request(Channel, RequestType, RequestID, Request, Controller, State) ->
		{Response, StateUpdateMessage, NewState} when
	Channel :: atom(),
	RequestType :: binary(),
	RequestID :: term(),
	Request :: term(),
	Controller :: module(),
	State :: term(),
	Response :: term(),
	StateUpdateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: term(),
	NewState :: State.

%%% -------------------------------------------------------------------------------------------------------------------

%%% @doc Called by the entity/client communication interface (pre_entity_comm) when the client sends an event that the
%%% controller needs to process. No response is expected.

-callback client_event(Channel, EventType, Event, Controller, State) -> {StateUpdateMessage, NewState} when
	Channel :: atom(),
	EventType :: binary(),
	Event :: term(),
	Controller :: module(),
	State :: term(),
	StateUpdateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: term(),
	NewState :: State.

%%% -------------------------------------------------------------------------------------------------------------------

%%% @doc Called by the entity engine whenever another entity sends or broadcasts an event. No response is expected.

-callback entity_event(Event, From, Controller, State) -> {StateUpdateMessage, NewState} when
	Event :: term(),
	From :: pid(),
	Controller :: module(),
	State :: term(),
	StateUpdateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: term(),
	NewState :: State.

%%% -------------------------------------------------------------------------------------------------------------------

%%% @doc Called by the entity engine and its worker process in order to update the local entity state with the given
%%% update. No response is expected.

-callback apply_update(Key, Value, Controller, State) -> {StateUpdateMessage, NewState} when
	Key :: atom(),
	Value :: term(),
	Controller :: module(),
	State :: term(),
	StateUpdateMessage :: [{Key, Value}],
	NewState :: State.
