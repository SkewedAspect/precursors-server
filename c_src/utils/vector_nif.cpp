/**
 * @doc Vector NIF module - provides a C++ implementation of the `vector` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <string.h>

#include "nif_helpers.h"

#include "vector_nif.h"


// --------------------------------------------------------------------------------------------------------------------

static bool termToVec(ErlNifEnv* env, ERL_NIF_TERM term, Vec& targetVec)
{
	int arity;
	const ERL_NIF_TERM* array;

	if(!enif_get_tuple(env, term, &arity, &array))
	{
		return false;
	} // end if

	if(arity != 4)
	{
		return false;
	} // end if

	if(!enif_get_double(env, array[0], &targetVec.x)
			|| !enif_get_double(env, array[1], &targetVec.y)
			|| !enif_get_double(env, array[2], &targetVec.z))
	{
		return false;
	} // end if

	return true;
} // termToVec


static ERL_NIF_TERM vecToTerm(ErlNifEnv* env, Vec vec)
{
	return enif_make_tuple3(env,
			enif_make_double(env, vec.x),
			enif_make_double(env, vec.y),
			enif_make_double(env, vec.z)
			);
} // end vecToTerm


// --------------------------------------------------------------------------------------------------------------------
// NIFs

// dot/2
static ERL_NIF_TERM dot(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2)
	FAIL;
} // end dot

// cross/2
static ERL_NIF_TERM cross(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2)
	FAIL;
} // end cross

// multiply/2
static ERL_NIF_TERM multiply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2)
	FAIL;
} // end multiply

// divide/2
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2)
	FAIL;
} // end divide

// squared_norm/1
static ERL_NIF_TERM squared_norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1)
	FAIL;
} // end squared_norm

// norm/1
static ERL_NIF_TERM norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1)
	FAIL;
} // end norm

// length/1
static ERL_NIF_TERM length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1)
	FAIL;
} // end length

// unit/1
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1)
	FAIL;
} // end unit

// hpr_to/1
static ERL_NIF_TERM hpr_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1)
	FAIL;
} // end hpr_to

// add/2, add/3
static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC_RANGE(2, 3)
	FAIL;
} // end add

// subtract/2
static ERL_NIF_TERM subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2)
	FAIL;
} // end subtract

// is_zero/1
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1)
	FAIL;
} // end is_zero
