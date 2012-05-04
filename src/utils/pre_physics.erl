%% ------------------------------------------------------------------------
%% @doc Physical module - simulates psuedo physical movement.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ------------------------------------------------------------------------

-module(pre_physics).

% -------------------------------------------------------------------------

% external api 
-export([simulate/2]).

% -------------------------------------------------------------------------

-include("log.hrl").
-include("pre_physics.hrl").

%% ------------------------------------------------------------------------
%% External API
%% ------------------------------------------------------------------------

-spec simulate(StepSize, State) -> State when
	StepSize :: float(),
	State :: #physical{}.

%% @doc Simulate physical movement of the 'physical' object represented by State, over the given StepSize.
simulate(StepSize, State) ->
	#physical{
		position = Position,
		position_vel = PositionVel,
		position_acc_abs = PositionAccAbs,
		position_acc_rel = PositionAccRel,
		orientation = Orientation,
		orientation_vel = OrientationVel,
		orientation_acc_abs = OrientationAccAbs,
		orientation_acc_rel = OrientationAccRel
	} = State,

	% ---------------------------------------------------------------------

	% Calculate total overall orientational accelleration, in world space
	OrientationAcc = quaternion:compose(OrientationAccAbs, quaternion:reorient(OrientationAccRel, Orientation)),

	% Calculate new orientational velocity, in world space
	NewOrientationVel = quaternion:compose(OrientationVel, quaternion:scale_rotation(StepSize, OrientationAcc)),

	% Calculate new orientation, in world space
	NewOrientation = quaternion:compose(Orientation, quaternion:scale_rotation(StepSize, NewOrientationVel)),

	% ---------------------------------------------------------------------
	
	% Calculate total overall positional accelleration, in world space
	PositionAcc = vector:add(PositionAccAbs, quaternion:rotate(PositionAccRel, NewOrientation)),

	% Calculate new positional velocity, in world space
	NewPositionVel = vector:add(PositionVel, vector:multiply(StepSize, PositionAcc)),

	% Calculate new position, in world space
	NewPosition = vector:add(Position, vector:multiply(StepSize, NewPositionVel)),

	% ---------------------------------------------------------------------

	% Return a new record, with updates state
	#physical{
		position = NewPosition,
		position_vel = NewPositionVel,
		position_acc_abs = PositionAccAbs,
		position_acc_rel = PositionAccRel,
		orientation = NewOrientation,
		orientation_vel = NewOrientationVel,
		orientation_acc_abs = OrientationAccAbs,
		orientation_acc_rel = OrientationAccRel
	}.
