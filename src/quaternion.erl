%% ------------------------------------------------------------------------
%% @doc Quaternion module - simplifies working with and perfoming math on quaternions.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ------------------------------------------------------------------------

-module(quaternion).

% -------------------------------------------------------------------------

% external api 
-export([quat_to_list/1, list_to_quat/1, add/2, subtract/2, multiply/2, divide/2, norm/1, length/1]).
-export([unit/1, conjugate/1, inverse/1, reciprocal/1, compose/2, relative_to/2, is_zero/1]).

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

%% @doc
conjugate({W, X, Y, Z}) ->
	{W, -X, -Y, -Z}.


%% @doc
inverse({_, _, _, _} = Quat) ->
	divide(conjugate(Quat), norm(Quat)).

% -------------------------------------------------------------------------

%% @doc Checks to see if this is a zero quaternion 
is_zero({0, 0, 0, 0}) ->
	true;

is_zero({_, _, _, _}) ->
	false.
