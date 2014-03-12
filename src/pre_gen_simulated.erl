%%% @doc The behaviour for all simulated object controller modules.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_gen_simulated).

%% --------------------------------------------------------------------------------------------------------------------
%% Behaviour
%% --------------------------------------------------------------------------------------------------------------------

%%% @doc Called when a new simulated object is created. It should set up the default state required by the controller.
%%% It should return an opaque state.

-callback init(EntityID, InitData) -> State when
	EntityID :: term(),
	InitData :: [{Key, Value}],
	Key :: atom(),
	Value :: any(),
	State :: any().

%%% -------------------------------------------------------------------------------------------------------------------

%%% @doc Called every simulation frame by the simulation engine. It is expected to return `{Update, NewState}' where
%%% `Update' is either a JSON structure (a proplist) representing any changes or `[]' if no changes occurred, and
%%% `NewState' is the new entity record that should be used for future simulation.

-callback simulate(Updates, State) -> {StateUpdateMessage, NewState} when
	Updates :: [UpdateSpec],
	UpdateSpec :: remove | {Key, Value},
	State :: any(),
	StateUpdateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: any(),
	NewState :: State.
