/**
 * @doc Physics: 4th-order Runge-Kutta integration - a C++ implementation of the `pre_physics_rk4` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

/* Erlang module exports:
 * - simulate/1
 * - simulate/2
 * - default_physical/0
 * - to_proplist/1
 * - from_proplist/1
 * - update_from_proplist/2
 * - get_prop/2
 */

/* Properties:
 * - position
 * - linear_momentum
 * - orientation
 * - angular_momentum
 *
 * - force_absolute
 * - force_relative
 * - torque_absolute
 * - torque_relative
 *
 * - last_update
 * - linear_velocity
 * - angular_velocity
 * - spin
 *
 * - mass
 * - inverse_mass
 * - inertia_tensor
 * - inverse_inertia_tensor
 */

#ifndef __PRE_PHYSICS_RK4_NIF__
#define __PRE_PHYSICS_RK4_NIF__

#include "Vec.h"


// --------------------------------------------------------------------------------------------------------------------
// NIFs

/**
 * Simulate physical movement of the 'physical' object represented by `InitialPhysical`, over the time since that
 * object's 'last_update' timestamp, or over the given `TimeDelta`.
 */
// simulate/1, simulate/2
// simulate(Physical) ->
// simulate(TimeDelta, Physical) ->
static ERL_NIF_TERM simulate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/**
 * Create a new 'physical' object, with its 'last_update' timestamp set to the current time.
 */
// default_physical() -> #physical{last_update = os:timestamp()}.
static ERL_NIF_TERM default_physical(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// to_proplist/1
// to_proplist(Physical) -> [{Key, Value}, ...]
static ERL_NIF_TERM to_proplist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// from_proplist/1
// from_proplist([{Key, Val}, ...]) ->
static ERL_NIF_TERM from_proplist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// update_from_proplist/2
// update_from_proplist(Physical, [{Key, Val}, ...]) ->
static ERL_NIF_TERM update_from_proplist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// get_prop/2
// get_prop(Key, Physical) ->
static ERL_NIF_TERM get_prop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


// --------------------------------------------------------------------------------------------------------------------
// Helpers


// --------------------------------------------------------------------------------------------------------------------
// Declare Erlang exports

static ErlNifFunc nif_funcs[] =
{
	{"simulate", 2, simulate}
};

ERL_NIF_INIT(pre_physics_rk4, nif_funcs, NULL, NULL, NULL, NULL)


#endif // __PRE_PHYSICS_RK4_NIF__
