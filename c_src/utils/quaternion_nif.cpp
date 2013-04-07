/**
 * @doc Quaternion NIF module - provides a C++ implementation of the `quaternion` Erlang module
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <string>

#include "angles.h"
#include "nif_helpers.h"

#include "quaternion_nif.h"


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

// quat_to_list/1
static ERL_NIF_TERM quat_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	int arity;
	const ERL_NIF_TERM* array;

	if(enif_get_tuple(env, argv[0], &arity, &array) && arity == 4)
	{
		return enif_make_list_from_array(env, array, 4);
	}
	else
	{
		FAIL;
	} // end if
} // end quat_to_list

// list_to_quat/1
static ERL_NIF_TERM list_to_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	ERL_NIF_TERM vecTuple[4];
	ERL_NIF_TERM current = argv[0], tail;

	for(size_t idx = 0; idx < 4; idx++)
	{
		if(!enif_get_list_cell(env, current, &vecTuple[idx], &tail))
		{
			FAIL;
		} // end if

		current = tail;
	} // end for

	if(enif_is_empty_list(env, tail))
	{
		return enif_make_tuple_from_array(env, vecTuple, 4);
	}
	else
	{
		FAIL;
	} // end if
} // end list_to_quat

// add/2
static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end add

// subtract/2
static ERL_NIF_TERM subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end subtract

// multiply/2
static ERL_NIF_TERM multiply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end multiply

// divide/2
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end divide

// reorient/2
static ERL_NIF_TERM reorient(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end reorient

// scale_rotation/2
static ERL_NIF_TERM scale_rotation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end scale_rotation

// norm/1
static ERL_NIF_TERM norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	FAIL;
} // end norm

// length/1
static ERL_NIF_TERM length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	FAIL;
} // end length

// unit/1
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	FAIL;
} // end unit

// conjugate/1
static ERL_NIF_TERM conjugate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	FAIL;
} // end conjugate

// inverse/1
static ERL_NIF_TERM inverse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	FAIL;
} // end inverse

// reciprocal/1
static ERL_NIF_TERM reciprocal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	FAIL;
} // end reciprocal

// compose/2
static ERL_NIF_TERM compose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end compose

// relative_to/2
static ERL_NIF_TERM relative_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end relative_to

// rotate/2
static ERL_NIF_TERM rotate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(2);
	FAIL;
} // end rotate

// from_axis_angle/2, from_axis_angle/3
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC_RANGE(2, 3);
	FAIL;
} // end from_axis_angle

// from_body_rates/1, from_body_rates/2
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC_RANGE(1, 2);
	FAIL;
} // end from_body_rates

// from_euler/1, from_euler/2
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC_RANGE(1, 2);
	FAIL;
} // end from_euler

// is_zero/1
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	FAIL;
} // end is_zero


//FIXME: Remove these from this module! If anything, they should be in an 'angles' module.

// rad2deg/1
static ERL_NIF_TERM rad2deg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	double radians;

	if(getNIFDouble(env, argv[0], &radians))
	{
		return enif_make_double(env, rad2deg(radians));
	}
	else
	{
		FAIL;
	} // end if
} // end rad2deg

// deg2rad/1
static ERL_NIF_TERM deg2rad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	CHECK_ARGC(1);
	double degrees;

	if(getNIFDouble(env, argv[0], &degrees))
	{
		return enif_make_double(env, deg2rad(degrees));
	}
	else
	{
		FAIL;
	} // end if
} // end deg2rad


// --------------------------------------------------------------------------------------------------------------------
// Helpers

static bool termToQuat(ErlNifEnv* env, const ERL_NIF_TERM term, Quat& targetQuat)
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

	if(!getNIFDouble(env, array[0], &targetQuat.w)
			|| !getNIFDouble(env, array[1], &targetQuat.x)
			|| !getNIFDouble(env, array[2], &targetQuat.y)
			|| !getNIFDouble(env, array[3], &targetQuat.z))
	{
		return false;
	} // end if

	return true;
} // termToQuat


static inline ERL_NIF_TERM quatToTerm(ErlNifEnv* env, const Quat& quat)
{
	return enif_make_tuple4(env,
			enif_make_double(env, quat.w),
			enif_make_double(env, quat.x),
			enif_make_double(env, quat.y),
			enif_make_double(env, quat.z)
			);
} // end quatToTerm
