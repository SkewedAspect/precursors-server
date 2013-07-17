/**
 * @doc Vector NIF module - provides a C++ implementation of the `vector` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

/* Erlang module exports:
 * - vec_to_list/1
 * - list_to_vec/1
 * - dot/2
 * - cross/2
 * - multiply/2
 * - divide/2
 * - squared_norm/1
 * - norm/1
 * - length/1
 * - unit/1
 * - hpr_to/1
 * - add/2
 * - add/3
 * - subtract/2
 * - is_zero/1
 */

#ifndef __VECTOR_NIF__
#define __VECTOR_NIF__

#include "Vec.h"


// --------------------------------------------------------------------------------------------------------------------
// NIF Inititialization

static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

static int nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);


// --------------------------------------------------------------------------------------------------------------------
// NIFs

// vec_to_list/1
static ERL_NIF_TERM vec_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// list_to_vec/1
static ERL_NIF_TERM list_to_vec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// dot/2
static ERL_NIF_TERM dot(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// cross/2
static ERL_NIF_TERM cross(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// multiply/2
static ERL_NIF_TERM multiply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// divide/2
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// squared_norm/1
static ERL_NIF_TERM squared_norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// norm/1
static ERL_NIF_TERM norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// length/1
static ERL_NIF_TERM length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// unit/1
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// hpr_to/1
static ERL_NIF_TERM hpr_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// add/2
static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// add/3
static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// subtract/2
static ERL_NIF_TERM subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// is_zero/1
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


// --------------------------------------------------------------------------------------------------------------------
// Declare Erlang exports

static ErlNifFunc nif_funcs[] =
{
	{"vec_to_list", 1, vec_to_list},
	{"list_to_vec", 1, list_to_vec},
	{"dot", 2, dot},
	{"cross", 2, cross},
	{"multiply", 2, multiply},
	{"divide", 2, divide},
	{"squared_norm", 1, squared_norm},
	{"norm", 1, norm},
	{"length", 1, length},
	{"unit", 1, unit},
	{"hpr_to", 1, hpr_to},
	{"add", 2, add},
	{"add", 3, add},
	{"subtract", 2, subtract},
	{"is_zero", 1, is_zero}
};

ERL_NIF_INIT(vector, nif_funcs, nif_load, NULL, nif_upgrade, NULL)


#endif // __VECTOR_NIF__
