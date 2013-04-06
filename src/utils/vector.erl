%% ------------------------------------------------------------------------
%% @doc Vector module - simplifies working with and perfoming math on vectors.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ------------------------------------------------------------------------

-module(vector).

% -------------------------------------------------------------------------

% external api
-export([vec_to_list/1, list_to_vec/1, dot/2, cross/2, multiply/2, divide/2, squared_norm/1, norm/1, length/1]).
-export([unit/1, hpr_to/1, add/2, add/3, subtract/2, is_zero/1]).

-export_type([vec/0]).

% A 3D vector (x + y + z)
-type vec() :: {
	X :: float(),
	Y :: float(),
	X :: float()
}.

% -------------------------------------------------------------------------

-define(NORMALIZED_TOLERANCE, 0.0000001).

% -------------------------------------------------------------------------

-include("log.hrl").

%% --------------------------------------------------------------------------------------------------------------------
%% NIF module
%% --------------------------------------------------------------------------------------------------------------------

% Don't enable this unless testing, or until _all_ functions are implemented in C++ too.
% (loading the NIF module actually replaces this module, so we lose the Erlang implementations if we load the C++ ones)
%-on_load(init/0).
init() ->
    NifPaths = [
		"./vector",
		"./ebin/vector",
		"../ebin/vector"
	],

	TryLoad = fun(NifPath) ->
		case erlang:load_nif(NifPath, 0) of
			{error, {load_failed, _}} -> false;
			ok -> true
		end
	end,

	case lists:any(TryLoad, NifPaths) of
		true ->
			ok;
		false ->
			?warning("Couldn't load NIF from any of the defined locations! Falling back to Erlang implementation.")
	end.



%% ------------------------------------------------------------------------
%% External API
%% ------------------------------------------------------------------------

%% @doc Convert from a vector to a list.
vec_to_list({X, Y, Z}) ->
	[X, Y, Z].


%% @doc Convert from a vector to a list.
list_to_vec([X, Y, Z]) ->
	{X, Y, Z}.

% -------------------------------------------------------------------------

%% @doc Perform dot product.
dot({X1, Y1, Z1}, {X2, Y2, Z2}) ->
	X1 * X2 + Y1 * Y2 + Z1 * Z2.

% -------------------------------------------------------------------------

%% @doc Perform cross product.
cross({X1, Y1, Z1}, {X2, Y2, Z2}) ->
	{
		Y1 * Z2 - Z1 * Y2,
		-X1 * Z2 + Z1 * X2,
		X1 * Y2 - Y1 * X2
	}.

% -------------------------------------------------------------------------

%% @doc Scales a vector by the given factor.
multiply(Factor, {X, Y, Z}) when is_number(Factor) ->
	{Factor * X, Factor * Y, Factor * Z}.


%% @doc Scales a vector by the given factor.
%% NOTE: This uses a guard clause instead of a pattern because floats and ints don't match each other in patterns.
divide({_, _, _}, Factor) when Factor == 0 ->
	{error, division_by_zero};

divide({X, Y, Z}, Factor) when is_number(Factor) ->
	{X / Factor, Y / Factor, Z / Factor}.

% -------------------------------------------------------------------------

%% @doc Returns the squared length of the vector. This is useful in some optimization cases, as it avoids a sqrt call.
squared_norm({X, Y, Z}) ->
	math:pow(X, 2) + math:pow(Y, 2) + math:pow(Z, 2).


%% @doc Returns the length of the vector.
norm(Vec) ->
	math:sqrt(squared_norm(Vec)).


%% @doc Returns the length of the vector.
length(Vec) ->
	norm(Vec).

% -------------------------------------------------------------------------

% @doc Returns a unit vector in the same direction as Vec.
unit({_, _, _} = Vec) ->
	VLS = squared_norm(Vec),
	unit(VLS, Vec).


%% @doc hidden
unit(VLS, {_, _, _} = Vec) when VLS < ?NORMALIZED_TOLERANCE; abs(VLS - 1) < ?NORMALIZED_TOLERANCE ->
	Vec;

%% @doc hidden
unit(VLS, {_, _, _} = Vec) ->
	divide(Vec, math:sqrt(VLS)).


%% @doc Gets the Yaw and Pitch required to point in the direction of the given vector.
hpr_to({X, Y, Z}) ->
	{X1, Y1, Z1} = unit({X, Y, Z}),
	Yaw = -math:atan2(X1, Y1),
	Pitch = math:atan2(Z1, math:sqrt(math:pow(X1, 2) + math:pow(Y1, 2))),

	{rad2deg(Yaw), rad2deg(Pitch), 0}.

% -------------------------------------------------------------------------

%% @doc Adds two vectors together.
add({X1, Y1, Z1}, {X2, Y2, Z2}) ->
	{X1 + X2, Y1 + Y2, Z1 + Z2}.


%% @doc Adds three vectors together.
add(V1, V2, V3) ->
	add(add(V1, V2), V3).


%% @doc Subtracts the second vector from the first.
subtract({X1, Y1, Z1}, {X2, Y2, Z2}) ->
	{X1 - X2, Y1 - Y2, Z1 - Z2}.

% -------------------------------------------------------------------------

%% @doc Checks to see if this is a zero vector.
%% NOTE: This uses a guard clause instead of a pattern because floats and ints don't match each other in patterns.
is_zero({X, Y, Z}) when X == 0, Y == 0, Z == 0 ->
	true;

is_zero({_, _, _}) ->
	false.

%% ------------------------------------------------------------------------
%% Internal API
%% ------------------------------------------------------------------------

%%% @doc Convert radians to degrees.
rad2deg(Radians) ->
	Radians * (180/math:pi()).
