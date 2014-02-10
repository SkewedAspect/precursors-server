/**
 * @doc Physics: 4th-order Runge-Kutta integration - a C++ implementation of the `pre_physics_rk4` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include "nif_helpers.h"
#include "vector_nif_utils.h"

#include "pre_physics_rk4_nif.h"


// --------------------------------------------------------------------------------------------------------------------
// NIFs

// simulate/2
static ERL_NIF_TERM simulate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
