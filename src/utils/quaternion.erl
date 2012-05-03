%% ------------------------------------------------------------------------
%% @doc Quaternion module - simplifies working with and perfoming math on quaternions.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ------------------------------------------------------------------------

-module(quaternion).

% -------------------------------------------------------------------------

% external api 
-export([quat_to_list/1, list_to_quat/1, add/2, subtract/2, multiply/2, divide/2, reorient/2]).
-export([scale_rotation/2, norm/1, length/1, unit/1, conjugate/1, inverse/1, reciprocal/1]).
-export([compose/2, relative_to/2, rotate/2, from_axis_angle/2, from_axis_angle/3]).
-export([from_body_rates/1, from_body_rates/2, from_euler/1, from_euler/2, rad2deg/1, deg2rad/1, is_zero/1]).

-export_type([quat/0]).

% A quaternion (w + xi + yj + zk)
-type quat() :: {
	W :: float(),
	X :: float(),
	Y :: float(),
	X :: float()
}.

% -------------------------------------------------------------------------

-define(NORMALIZED_TOLERANCE, 0.0000001).
-define(IDENTITY, {1, 0, 0, 0}).

% -------------------------------------------------------------------------

-include("log.hrl").

%% ------------------------------------------------------------------------
%% External API
%% ------------------------------------------------------------------------

%% @doc Convert from a quaternion to a list
quat_to_list({W, X, Y, Z}) ->
	[W, X, Y, Z].


%% @doc Convert from a quaternion to a list
list_to_quat([W, X, Y, Z]) ->
	{W, X, Y, Z}.

% -------------------------------------------------------------------------

%% @doc Adds the two quaternions together.
add({W1, X1, Y1, Z1}, {W2, X2, Y2, Z2}) ->
	{W1 + W2, X1 + X2, Y1 + Y2, Z1 + Z2}.


%% @doc Subtracts the second quaternion from the first.
subtract({W1, X1, Y1, Z1}, {W2, X2, Y2, Z2}) ->
	{W1 - W2, X1 - X2, Y1 - Y2, Z1 - Z2}.

% -------------------------------------------------------------------------

%% @doc Quaternion Multiplication
multiply(Factor, {W, X, Y, Z}) when is_integer(Factor); is_float(Factor) ->
	{Factor * W, Factor * X, Factor * Y, Factor * Z};

multiply({_W, _X, _Y, _Z} = Quat, Factor) when is_integer(Factor); is_float(Factor) ->
	multiply(Factor, Quat);

multiply({W1, X1, Y1, Z1}, {W2, X2, Y2, Z2}) ->
	%(Q1 * Q2).w = (w1w2 - x1x2 - y1y2 - z1z2)
	%(Q1 * Q2).x = (w1x2 + x1w2 + y1z2 - z1y2)
	%(Q1 * Q2).y = (w1y2 - x1z2 + y1w2 + z1x2)
	%(Q1 * Q2).z = (w1z2 + x1y2 - y1x2 + z1w2)

	{
		W1 * W2 - X1 * X2 - Y1 * Y2 - Z1 * Z2,
		W1 * X2 + X1 * W2 + Y1 * Z2 - Z1 * Y2,
		W1 * Y2 - X1 * Z2 + Y1 * W2 + Z1 * X2,
		W1 * Z2 + X1 * Y2 - Y1 * X2 + Z1 * W2
	}.


%% @doc Scales the quaternion by the given factor.
divide(0, {_, _, _, _}) ->
	{error, division_by_zero};

divide(Factor, {W, X, Y, Z}) when is_number(Factor) ->
	{W / Factor, X / Factor, Y / Factor, Z / Factor}.

% -------------------------------------------------------------------------

%% @doc Reorient q1's axis of rotation by rotating it by q2, but leave q1's angle of rotation intact.
reorient({W, X, Y, Z}, {_, _, _, _}=Q2) ->
	OriginalRotation = 2 * math:acos(W),
	Axis = rotate(vector:unit({X, Y, Z}), Q2),
	from_axis_angle(Axis, OriginalRotation).


%% @doc Scale the rotation of the quaternion by the given factor. Note: This is not the same as multiplying.
scale_rotation(Factor, {W, X, Y, Z}) when is_integer(Factor); is_float(Factor) ->
	OriginalRotation = 2 * math:acos(W),
	Unit = vector:unit({X, Y, Z}),
	from_axis_angle(Unit, OriginalRotation * Factor).

% -------------------------------------------------------------------------

%% @doc Returns the squared length of the quaternion. This is useful in some optimization cases, as it avoids a sqrt call.
squared_norm({W, X, Y, Z}) ->
	math:pow(W, 2) + math:pow(X, 2) + math:pow(Y, 2) + math:pow(Z, 2).


%% @doc Returns the length of the quaternion.
norm({_, _, _, _} = Quat) ->
	math:sqrt(squared_norm(Quat)).


%% @doc Returns the length of the quaternion.
length(Quat) ->
	norm(Quat).

% -------------------------------------------------------------------------

% @doc Returns a unit vector in the same direction as Quat.
unit({_, _, _, _} = Quat) ->
	QLS = squared_norm(Quat),
	unit(QLS, Quat).


%% @doc hidden
unit(0, {_, _, _, _} = Quat) ->
	Quat;

%% @doc hidden
unit(QLS, {_, _, _, _} = Quat) ->
	Norm = abs(QLS - 1.0),
	case Norm < ?NORMALIZED_TOLERANCE of
		true ->
			Quat;
		_ ->
			divide(math:sqrt(QLS), Quat)
	end.

% -------------------------------------------------------------------------

%% @doc
conjugate({W, X, Y, Z}) ->
	{W, -X, -Y, -Z}.


%% @doc
inverse({_, _, _, _} = Quat) ->
	divide(conjugate(Quat), norm(Quat)).

% -------------------------------------------------------------------------

%% @doc
reciprocal({_, _, _, _} = Quat) ->
	divide(conjugate(Quat), squared_norm(quat)).

%% @doc Get the quaternion which results from composing the rotations represented by `first` and `second`.
compose({_, _, _, _} = First, {_, _, _, _} = Second) ->
	multiply(First, Second).

% -------------------------------------------------------------------------

%% @doc Get the quaternion representing the orientation of `target` relative to `reference`.
relative_to({_, _, _, _} = Target, {_, _, _, _} = Reference) ->
	multiply(multiply(Reference, Target), conjugate(Reference)).

% -------------------------------------------------------------------------

%% @doc Rotates the vector by Rotation.
rotate({X, Y, Z}, {_, _, _, _} = Rotation) ->
	{_, X1, Y1, Z1} = relative_to({0, X, Y, Z}, Rotation),
	{X1, Y1, Z1}.

% -------------------------------------------------------------------------

%% @doc Converts from and axis and angle (radians), to a quaternion.
from_axis_angle({_, _, _} = Axis, Angle) when is_number(Angle) ->
	from_axis_angle(radians, Axis, Angle).


%% @doc Converts from and axis and angle (radians), to a quaternion.
from_axis_angle(radians, {_, _, _} = Axis, Angle) when is_number(Angle) ->
	ComplexFactor = math:sin(Angle / 2),
	{X, Y, Z} = vector:multiply(ComplexFactor, Axis),
	{math:cos(Angle / 2), X, Y, Z};

%% @doc Converts from and axis and angle (degrees), to a quaternion.
from_axis_angle(degrees, Axis, Angle) when is_number(Angle) ->
	DegAngle = deg2rad(Angle),
	from_axis_angle(radians, {_, _, _} = Axis, DegAngle).

% -------------------------------------------------------------------------

%% @doc Converts from body rates (radians) to a quaternion.
from_body_rates({_, _, _} = Vec) ->
	from_body_rates(radians, Vec).


%% @doc Converts from body rates (radians) to a quaternion.
from_body_rates(radians, {X, Y, Z} = Vec) ->
	case vector:is_zero(Vec) of
		true ->
			?IDENTITY;

		_ ->
			Vec1 = {Y, Z, X},
			Speed = vector:norm(Vec1),
			Axis = vector:divide(Speed, Vec1),
			from_axis_angle(Axis, Speed)
	end;

%% @doc Converts from body rates (degrees) to a quaternion.
from_body_rates(degrees, {X, Y, Z}) ->
	from_body_rates(radians, {deg2rad(X), deg2rad(Y), deg2rad(Z)}). 

% -------------------------------------------------------------------------

%% @doc Converts from a vector of euler angles (radians) to a quaternion.
from_euler({_, _, _} = Vec) ->
	from_euler(radians, Vec).


%% @doc Converts from a vector of euler angles (radians) to a quaternion.
from_euler(radians, {Yaw, Pitch, Roll}) ->
	HalfYaw = Yaw / 2,
	HalfPitch = Pitch / 2,
	HalfRoll = Roll / 2,

	{
		math:cos(HalfPitch) * math:cos(HalfRoll) * math:cos(HalfYaw) +
				math:sin(HalfPitch) * math:sin(HalfRoll) * math:sin(HalfYaw), 

		math:sin(HalfPitch) * math:cos(HalfRoll) * math:cos(HalfYaw) -
				math:cos(HalfPitch) * math:sin(HalfRoll) * math:sin(HalfYaw), 
		
		math:cos(HalfPitch) * math:sin(HalfRoll) * math:cos(HalfYaw) +
				math:sin(HalfPitch) * math:cos(HalfRoll) * math:sin(HalfYaw), 
		
		math:cos(HalfPitch) * math:cos(HalfRoll) * math:sin(HalfYaw) +
				math:sin(HalfPitch) * math:sin(HalfRoll) * math:cos(HalfYaw) 
	};

%% @doc Converts from a vector of euler angles (degrees) to a quaternion.
from_euler(degrees, {Yaw, Pitch, Roll}) ->
	from_euler(radians, {deg2rad(Yaw), deg2rad(Pitch), deg2rad(Roll)}).

% -------------------------------------------------------------------------

%% @doc Checks to see if this is a zero quaternion 
is_zero({0, 0, 0, 0}) ->
	true;

is_zero({_, _, _, _}) ->
	false.

%% ------------------------------------------------------------------------
%% Internal API
%% ------------------------------------------------------------------------

%%% @doc Convert radians to degrees.
rad2deg(Radians) ->
	Radians * (180/math:pi()).

%%% @doc Convert radians to degrees.
deg2rad(Degrees) ->
	Degrees * (math:pi()/180).
