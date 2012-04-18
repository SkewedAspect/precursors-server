%%% @doc An entity representing a physical object.

-module(entity_physical).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([get_full_state/1]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

get_full_state(Entity) ->
	#physical{
		position = Position,
		position_vel = PositionVel,
		position_acc_abs = PositionAccAbs,
		position_acc_rel = PositionAccRel,
		orientation = Orientation,
		orientation_vel = OrientationVel,
		orientation_acc_abs = OrientationAccAbs,
		orientation_acc_rel = OrientationAccRel
	} = Entity#entity.physical,

	FullState = [
		{position, vector:vec_to_list(Position)},
		{position_vel, vector:vec_to_list(PositionVel)},
		{position_acc_abs, vector:vec_to_list(PositionAccAbs)},
		{position_acc_rel, vector:vec_to_list(PositionAccRel)},
		{orientation, quaternion:quat_to_list(Orientation)},
		{orientation_vel, quaternion:quat_to_list(OrientationVel)},
		{orientation_acc_abs, quaternion:quat_to_list(OrientationAccAbs)},
		{orientation_acc_rel, quaternion:quat_to_list(OrientationAccRel)}
	],

	{FullState, Entity}.
