/**
 * @doc Vector NIF module - provides a C++ implementation of the `vector` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <string>

#include "nif_helpers.h"

#include "vector_nif.h"


static ERL_NIF_TERM true_atom;
static ERL_NIF_TERM false_atom;


// --------------------------------------------------------------------------------------------------------------------
// NIF Inititialization

static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	true_atom = enif_make_atom_len(env, "true", 4);
	false_atom = enif_make_atom_len(env, "false", 5);

	return 0;
} // end nif_load

static int nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	true_atom = enif_make_atom_len(env, "true", 4);
	false_atom = enif_make_atom_len(env, "false", 5);

	return 0;
} // end nif_upgrade


// --------------------------------------------------------------------------------------------------------------------
// NIFs

// dot/2
static ERL_NIF_TERM dot(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	Vec vec0, vec1;

	if(termToVec(env, argv[0], vec0) && termToVec(env, argv[1], vec1))
	{
		return enif_make_double(env, vec0.dot(vec1));
	}
	else
	{
		FAIL;
	} // end if
} // end dot

// cross/2
static ERL_NIF_TERM cross(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	Vec vec0, vec1;

	if(termToVec(env, argv[0], vec0) && termToVec(env, argv[1], vec1))
	{
		return vecToTerm(env, vec0.cross(vec1));
	}
	else
	{
		FAIL;
	} // end if
} // end cross

// multiply/2
static ERL_NIF_TERM multiply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	Vec vec;
	double factor;

	if(termToVec(env, argv[0], vec) && getNIFDouble(env, argv[1], &factor))
	{
		return vecToTerm(env, vec *= factor);
	}
	else if(getNIFDouble(env, argv[0], &factor) && termToVec(env, argv[1], vec))
	{
		return vecToTerm(env, vec *= factor);
	}
	else
	{
		FAIL;
	} // end if
} // end multiply

// divide/2
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	Vec vec;
	double divisor;

	if(termToVec(env, argv[0], vec) && getNIFDouble(env, argv[1], &divisor))
	{
		return vecToTerm(env, vec /= divisor);
	}
	else
	{
		FAIL;
	} // end if
} // end divide

// squared_norm/1
static ERL_NIF_TERM squared_norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	Vec vec;

	if(termToVec(env, argv[0], vec))
	{
		return enif_make_double(env, vec.squaredNorm());
	}
	else
	{
		FAIL;
	} // end if
} // end squared_norm

// norm/1
static ERL_NIF_TERM norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	Vec vec;

	if(termToVec(env, argv[0], vec))
	{
		return enif_make_double(env, vec.norm());
	}
	else
	{
		FAIL;
	} // end if
} // end norm

// length/1
static ERL_NIF_TERM length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	Vec vec;

	if(termToVec(env, argv[0], vec))
	{
		return enif_make_double(env, vec.norm()); // length just forwards to norm, so we'll use norm.
	}
	else
	{
		FAIL;
	} // end if
} // end length

// unit/1
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	Vec vec;

	if(termToVec(env, argv[0], vec))
	{
		return vecToTerm(env, vec.normalize()); // using normalize instead of unit avoids constructing another Vec
	}
	else
	{
		FAIL;
	} // end if
} // end unit

// hpr_to/1
static ERL_NIF_TERM hpr_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	Vec vec;

	if(termToVec(env, argv[0], vec))
	{
		return vecToTerm(env, vec.hprAlong());
	}
	else
	{
		FAIL;
	} // end if
} // end hpr_to

// add/2, add/3
static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC_RANGE(2, 3)
	Vec vec0, vec1;

	if(termToVec(env, argv[0], vec0) && termToVec(env, argv[1], vec1))
	{
		if(argc == 2)
		{
			return vecToTerm(env, vec0 += vec1);
		}
		else
		{
			Vec vec2;
			if(argc == 3 && termToVec(env, argv[2], vec2))
			{
				return vecToTerm(env, vec0 += vec1 += vec2);
			} // end if
		} // end if
	} // end if

	FAIL;
} // end add

// subtract/2
static ERL_NIF_TERM subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	Vec vec0, vec1;

	if(termToVec(env, argv[0], vec0) && termToVec(env, argv[1], vec1))
	{
		return vecToTerm(env, vec0 -= vec1);
	}
	else
	{
		FAIL;
	} // end if
} // end subtract

// is_zero/1
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	Vec vec;

	if(termToVec(env, argv[0], vec))
	{
		return vec.isZero() ?  true_atom : false_atom;
	}
	else
	{
		FAIL;
	} // end if
} // end is_zero


// --------------------------------------------------------------------------------------------------------------------
// Helpers

static bool termToVec(ErlNifEnv* env, const ERL_NIF_TERM term, Vec& targetVec)
{
	int arity;
	const ERL_NIF_TERM* array;

	if(!enif_get_tuple(env, term, &arity, &array))
	{
		return false;
	} // end if

	if(arity != 3)
	{
		return false;
	} // end if

	if(!getNIFDouble(env, array[0], &targetVec.x)
			|| !getNIFDouble(env, array[1], &targetVec.y)
			|| !getNIFDouble(env, array[2], &targetVec.z))
	{
		return false;
	} // end if

	return true;
} // termToVec


static inline ERL_NIF_TERM vecToTerm(ErlNifEnv* env, const Vec& vec)
{
	return enif_make_tuple3(env,
			enif_make_double(env, vec.x),
			enif_make_double(env, vec.y),
			enif_make_double(env, vec.z)
			);
} // end vecToTerm
