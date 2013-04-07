/**
 * @doc Vector NIF module - provides a C++ implementation of the `vector` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <string>

#include "nif_helpers.h"
#include "vector_nif_utils.h"

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

// vec_to_list/1
static ERL_NIF_TERM vec_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	int arity;
	const ERL_NIF_TERM* array;

	if(enif_get_tuple(env, argv[0], &arity, &array) && arity == 3)
	{
		return enif_make_list_from_array(env, array, 3);
	}
	else
	{
		FAIL;
	} // end if
} // end vec_to_list

// list_to_vec/1
static ERL_NIF_TERM list_to_vec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	ERL_NIF_TERM vecTuple[3];
	ERL_NIF_TERM current = argv[0], tail;

	for(size_t idx = 0; idx < 3; idx++)
	{
		if(!enif_get_list_cell(env, current, &vecTuple[idx], &tail))
		{
			FAIL;
		} // end if

		current = tail;
	} // end for

	if(enif_is_empty_list(env, tail))
	{
		return enif_make_tuple_from_array(env, vecTuple, 3);
	}
	else
	{
		FAIL;
	} // end if
} // end list_to_vec

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
