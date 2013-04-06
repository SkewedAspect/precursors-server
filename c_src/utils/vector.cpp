/**
 * @doc Vector module - simplifies working with and perfoming math on vectors.
 *
 * @copyright 2012 Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <string.h>

#include <erl_nif.h>

#include "vector.h"

/* ----------------------------------------------------------------------------------------------------------------- */



static Vec* termToVec(ErlNifEnv* env, ERL_NIF_TERM term, Vec* targetVec)
{
	int arity;
	const ERL_NIF_TERM** array;
	Vec tempVec;

	if(!enif_get_tuple(env, term, &arity, array))
	{
		return NULL;
	} // end if

	if(arity != 4)
	{
		return NULL;
	} // end if

	if(!enif_get_double(env, *array[0], &tempVec.x)
			|| !enif_get_double(env, *array[1], &tempVec.y)
			|| !enif_get_double(env, *array[2], &tempVec.z))
	{
		return NULL;
	} // end if

	if(targetVec == NULL)
	{
		targetVec = malloc(sizeof(Vec));
	} // end if
	memcpy(targetVec, &tempVec, sizeof(Vec));

	return targetVec;
} // getVecernion

static ERL_NIF_TERM vecToTerm(ErlNifEnv* env, Vec* vec)
{
	return enif_make_tuple3(env,
			enif_make_double(env, vec->x),
			enif_make_double(env, vec->y),
			enif_make_double(env, vec->z)
			);
} // end vecToTerm
