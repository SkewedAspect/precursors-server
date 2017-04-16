%%% @doc An entity representing a physical object.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_sim_physical).

-behaviour(pre_gen_simulated).

% API
-export([get_orientation/1, get_linear_velocity/1, get_angular_velocity/1, get_force_relative/1, get_torque_relative/1]).

% pre_gen_simulated
-export([init/2, simulate/2]).

-record(state, {
	id :: term(),
	physical :: any()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

get_orientation(State) ->
	pre_physics_rk4:get_prop(orientation, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_linear_velocity(State) ->
	pre_physics_rk4:get_prop(linear_velocity, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_angular_velocity(State) ->
	pre_physics_rk4:get_prop(angular_velocity, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_force_relative(State) ->
	pre_physics_rk4:get_prop(force_relative, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------

get_torque_relative(State) ->
	pre_physics_rk4:get_prop(torque_relative, State#state.physical).

%% --------------------------------------------------------------------------------------------------------------------
%% pre_gen_simulated
%% --------------------------------------------------------------------------------------------------------------------

-spec init(EntityID, InitData) -> State when
	EntityID :: term(),
	InitData :: [{Key, Value}],
	Key :: atom(),
	Value :: any(),
	State :: any().

init(EntityID, InitData) ->
	#state{
		id = EntityID,
		physical = pre_physics_rk4:update_from_proplist(pre_physics_rk4:default_physical(), InitData)
	}.

%% --------------------------------------------------------------------------------------------------------------------

-spec simulate(Updates, State) -> {StateUpdateMessage, NewState} when
	Updates :: [UpdateSpec],
	UpdateSpec :: remove | {Key, Value},
	State :: any(),
	StateUpdateMessage :: [{Key, Value}],
	Key :: atom(),
	Value :: any(),
	NewState :: State.

simulate(Updates, State) ->
	Physical1 = State#state.physical,

	%TODO: Update pre_physics_rk4:update_from_proplist/2 to handle the following updates:
	% - {update, position, vector:vec()}
	% - {update, linear_momentum, vector:vec()}
	% - {update, orientation, quaternion:quat()}
	% - {update, angular_momentum, vector:vec()}
	% - {apply, force_absolute, vector:vec()}
	% - {apply, force_relative, vector:vec()}
	% - {apply, torque_absolute, vector:vec()}
	% - {apply, torque_relative, vector:vec()}
	Physical2 = pre_physics_rk4:update_from_proplist(Physical1, Updates),

	% Do physics simulation
	Physical3 = pre_physics_rk4:simulate(Physical2),

	% Calculate a state update message
	CurPhysicalPL = pre_physics_rk4:to_proplist(Physical3),
	PrevPhysicalPL = pre_physics_rk4:to_proplist(Physical1),
	UpdateMsg = diff_proplist(PrevPhysicalPL, CurPhysicalPL),

	% Save State
	NewState = State#state{
		physical = Physical3
	},

	{UpdateMsg, NewState}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------------------------------------------------------

diff_proplist(OldState, NewState) ->
	lists:filter(
		fun({Key, NewVal}) ->
			case proplists:get_value(Key, OldState) of
				NewVal -> false;
				_ -> true
			end
		end,
		NewState
	).
