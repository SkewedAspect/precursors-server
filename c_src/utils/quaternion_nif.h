/**
 * @doc Quaternion NIF module - provides a C++ implementation of the `quaternion` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

/* Erlang module exports:
 *
 *		+ => implemented
 *		! => still need to implement
 *		- => don't bother implementing
 *		? => not sure yet
 *
 *   .-- Main C++ function
 *  / .- Erlang wrapper function
 * / /
 * - ! quat_to_list/1
 * - ! list_to_quat/1
 * ! ! add/2
 * ! ! subtract/2
 * ! ! multiply/2
 * ! ! divide/2
 * ! ! reorient/2
 * ! ! scale_rotation/2
 * ! ! norm/1
 * ! ! length/1
 * ! ! unit/1
 * ! ! conjugate/1
 * ! ! inverse/1
 * ! ! reciprocal/1
 * ! ! compose/2
 * ! ! relative_to/2
 * ! ! rotate/2
 * ! ! from_axis_angle/2
 * ! ! from_axis_angle/3
 * ! ! from_body_rates/1
 * ! ! from_body_rates/2
 * ! ! from_euler/1
 * ! ! from_euler/2
 * ! ! is_zero/1
 * + ? rad2deg/1
 * + ? deg2rad/1
 */

#ifndef __QUATERNION_NIF__
#define __QUATERNION_NIF__

#include "Quat.h"


// --------------------------------------------------------------------------------------------------------------------
// NIF Inititialization

static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

static int nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);


// --------------------------------------------------------------------------------------------------------------------
// NIFs

// quat_to_list/1
static ERL_NIF_TERM quat_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// list_to_quat/1
static ERL_NIF_TERM list_to_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// add/2
static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// subtract/2
static ERL_NIF_TERM subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// multiply/2
static ERL_NIF_TERM multiply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// divide/2
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// reorient/2
static ERL_NIF_TERM reorient(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// scale_rotation/2
static ERL_NIF_TERM scale_rotation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// norm/1
static ERL_NIF_TERM norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// length/1
static ERL_NIF_TERM length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// unit/1
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// conjugate/1
static ERL_NIF_TERM conjugate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// inverse/1
static ERL_NIF_TERM inverse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// reciprocal/1
static ERL_NIF_TERM reciprocal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// compose/2
static ERL_NIF_TERM compose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// relative_to/2
static ERL_NIF_TERM relative_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// rotate/2
static ERL_NIF_TERM rotate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// from_axis_angle/2
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// from_axis_angle/3
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// from_body_rates/1
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// from_body_rates/2
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// from_euler/1
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// from_euler/2
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// is_zero/1
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


//FIXME: Remove these from this module! If anything, they should be in an 'angles' module.
// rad2deg/1
static ERL_NIF_TERM rad2deg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
// deg2rad/1
static ERL_NIF_TERM deg2rad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


// --------------------------------------------------------------------------------------------------------------------
// Helpers

/*
// Conversion
static bool termToQuat(ErlNifEnv* env, const ERL_NIF_TERM term, Quat& targetQuat);

static inline ERL_NIF_TERM quatToTerm(ErlNifEnv* env, const Quat& quat);
*/


// --------------------------------------------------------------------------------------------------------------------
// Declare Erlang exports

static ErlNifFunc nif_funcs[] =
{
	{"quat_to_list", 1, quat_to_list},
	{"list_to_quat", 1, list_to_quat},
	{"add", 2, add},
	{"subtract", 2, subtract},
	{"multiply", 2, multiply},
	{"divide", 2, divide},
	{"reorient", 2, reorient},
	{"scale_rotation", 2, scale_rotation},
	{"norm", 1, norm},
	{"length", 1, length},
	{"unit", 1, unit},
	{"conjugate", 1, conjugate},
	{"inverse", 1, inverse},
	{"reciprocal", 1, reciprocal},
	{"compose", 2, compose},
	{"relative_to", 2, relative_to},
	{"rotate", 2, rotate},
	{"from_axis_angle", 2, from_axis_angle},
	{"from_axis_angle", 3, from_axis_angle},
	{"from_body_rates", 1, from_body_rates},
	{"from_body_rates", 2, from_body_rates},
	{"from_euler", 1, from_euler},
	{"from_euler", 2, from_euler},
	{"is_zero", 1, is_zero},

	//FIXME: Remove these from this module! If anything, they should be in an 'angles' module.
	{"rad2deg", 1, rad2deg},
	{"deg2rad", 1, deg2rad}
};

ERL_NIF_INIT(quaternion, nif_funcs, nif_load, NULL, nif_upgrade, NULL)


#endif // __QUATERNION_NIF__
