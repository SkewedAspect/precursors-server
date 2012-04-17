%%% @doc The test entity!

-module(entity_test).

-include("log.hrl").
-include("pre_entity.hrl").

% gen_server
-export([get_full_state/1, client_request/4, timer_fired/2]).

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

	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,

	FullState = [
		{position, vector:vec_to_list(Position)},
		{position_vel, vector:vec_to_list(PositionVel)},
		{position_acc_abs, vector:vec_to_list(PositionAccAbs)},
		{position_acc_rel, vector:vec_to_list(PositionAccRel)},
		{orientation, quaternion:quat_to_list(Orientation)},
		{orientation_vel, quaternion:quat_to_list(OrientationVel)},
		{orientation_acc_abs, quaternion:quat_to_list(OrientationAccAbs)},
		{orientation_acc_rel, quaternion:quat_to_list(OrientationAccRel)},
		{behavior, <<"Physical">>}
	],

	{ok, {Timestamp, FullState}, Entity}.

%FIXME: Put in base behavior:
%get_full_state(Entity) ->
%	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
%	Timestamp = MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
%	#entity{
%		id = #entity_id{
%			engine = EnginePid,
%			ref = EntityRef
%		}
%	} = Entity,
%	FullState = [
%		{id, [EnginePid, EntityRef]},
%		{timestamp, Timestamp}
%	],
%	{ok, FullState, Entity}.

%% -------------------------------------------------------------------

client_request(_RequestType, RequestID, _Request, Entity) ->
	ClientInfo = Entity#entity.client,
	Connection = ClientInfo#client_info.connection,
	Response = <<"Bumcovers.">>,
	pre_client_connection:send(Connection, tcp, {response, RequestID}, <<"input">>, Response),
	{ok, Response, Entity}.
	%{noreply, Entity}.

%% -------------------------------------------------------------------

timer_fired(TimerRef, Entity) ->
	ok.
