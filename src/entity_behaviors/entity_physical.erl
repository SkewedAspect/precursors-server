%%% @doc An entity representing a physical object.

-module(entity_physical).

-include("log.hrl").
-include("pre_entity.hrl").

% pre_entity
-export([init/2, get_full_state/1, client_request/5, timer_fired/2]).

-define(STEP_SIZE, 50).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

init(EntityID, Behavior) ->
	pre_entity_engine:start_entity_timer(EntityID, ?STEP_SIZE, do_physics),
	#entity{
		id = EntityID,
		callback_module = Behavior,
		physical = #physical{
			last_update = os:timestamp()
		}
	}.

%% -------------------------------------------------------------------

get_full_state(EntityState) ->
	#physical{
		position = Position,
		position_vel = PositionVel,
		position_acc_abs = PositionAccAbs,
		position_acc_rel = PositionAccRel,
		orientation = Orientation,
		orientation_vel = OrientationVel,
		orientation_acc_abs = OrientationAccAbs,
		orientation_acc_rel = OrientationAccRel
	} = EntityState#entity.physical,

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

	{FullState, EntityState}.

%% -------------------------------------------------------------------

client_request(EntityState, Channel, RequestType, _RequestID, Request) ->
	?warning("~p received invalid request ~p on channel ~p! (full request: ~p)",
		[EntityState#entity.id, RequestType, Channel, Request]),
	{error, invalid_request, EntityState}.

%% -------------------------------------------------------------------

timer_fired(EntityState, do_physics) ->
	#entity{
		physical = #physical{
			last_update = LastUpdate
		} = LastPhysical
	} = EntityState,
	ThisUpdate = os:timestamp(),
	Physical = pre_physics:simulate(timer:now_diff(ThisUpdate, LastUpdate) / 1000000, LastPhysical),
	EntityState1 = EntityState#entity{
		physical = Physical#physical{
			last_update = ThisUpdate
		}
	},
	{noreply, EntityState1}.
