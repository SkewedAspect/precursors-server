/**
 * @doc Quaternion module - simplifies working with and perfoming math on quaternions.
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <math.h>
#include <string.h>

#include <erl_nif.h>

#include "quaternion.h"


/* ----------------------------------------------------------------------------------------------------------------- */
/* Some useful constants */

#define TRUE 1

#define NORMALIZED_TOLERANCE 0.0000001
#define IDENTITY {1, 0, 0, 0}


/* ----------------------------------------------------------------------------------------------------------------- */
/* Metaprogramming is my second best friend. */

/**
 * Fail with a 'badarg' exception.
 */
#define FAIL return enif_make_badarg(env)

/**
 * Fail with a 'badarg' exception if the given expression is false.
 */
#define FAIL_IF(EXPR) if(EXPR) { FAIL; }

/**
 * If SAFER_THAN_NECESSARY is defined, CHECK_ARGC(N) ensures that the correct number of arguments are present in an
 * Erlang interface function.
 */
#ifdef SAFER_THAN_NECESSARY
#	define CHECK_ARGC(N) FAIL_IF(argc != N)
#else
#	define CHECK_ARGC(N)
#endif


/**
 * Define a function that computes a new Quat where each coordinate is calculated with `q1.coord OP q2.coord`.
 */
#define PAIRWISE_OP__TO_QUAT(NAME, OP) \
	static Quat NAME(const Quat q1, const Quat q2) \
	{ \
		Quat result; \
		result.w = q1.w OP q2.w; \
		result.x = q1.x OP q2.x; \
		result.y = q1.y OP q2.y; \
		result.z = q1.z OP q2.z; \
		return result; \
	}


/**
 * Create a NIF which wraps a C function with the signature: Quat function(Quat, Quat)
 */
#define ERL_WRAPPER_Q_Q_TO_Q(NAME) \
	static ERL_NIF_TERM NAME##_erl(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[]) \
	{ \
		Quat inputQuat1; \
		Quat inputQuat2; \
		Quat resultQuat; \
		\
		CHECK_ARGC(2); \
		FAIL_IF(!termToQuat(env, argv[0], &inputQuat1)) \
		FAIL_IF(!termToQuat(env, argv[1], &inputQuat2)) \
		\
		resultQuat = NAME(inputQuat1, inputQuat2); \
		\
		return quatToTerm(env, resultQuat); \
	}


/* ====================================================================================================================
 * C-only API
 * ================================================================================================================= */

static Quat* termToQuat(ErlNifEnv* env, ERL_NIF_TERM term, Quat* targetQuat)
{
	int arity;
	const ERL_NIF_TERM** array;
	Quat tempQuat;

	if(!enif_get_tuple(env, term, &arity, array))
	{
		return NULL;
	} // end if

	if(arity != 4)
	{
		return NULL;
	} // end if

	if(!enif_get_double(env, *array[0], &tempQuat.w)
			|| !enif_get_double(env, *array[1], &tempQuat.x)
			|| !enif_get_double(env, *array[2], &tempQuat.y)
			|| !enif_get_double(env, *array[3], &tempQuat.z))
	{
		return NULL;
	} // end if

	if(targetQuat == NULL)
	{
		targetQuat = malloc(sizeof(Quat));
	} // end if
	memcpy(targetQuat, &tempQuat, sizeof(Quat));

	return targetQuat;
} // getQuaternion

static ERL_NIF_TERM quatToTerm(ErlNifEnv* env, Quat quat)
{
	return enif_make_tuple4(env,
			enif_make_double(env, quat.w),
			enif_make_double(env, quat.x),
			enif_make_double(env, quat.y),
			enif_make_double(env, quat.z)
			);
} // end quatToTerm


/**
 * @doc Adds the two quaternions together.
 */
PAIRWISE_OP__TO_QUAT(quat_add, +)

/**
 * @doc Subtracts the second quaternion from the first.
 */
PAIRWISE_OP__TO_QUAT(quat_subtract, -)


// Because C doesn't have templates or polymorphic numeric types...
#define QUAT_MULTIPLY_FACTOR(FACTOR_TYPE) \
	static void quat_multiply_factor_##FACTOR_TYPE(Quat quat, FACTOR_TYPE factor, Quat* resultQuat) \
	{ \
		resultQuat->w = quat.w * factor; \
		resultQuat->x = quat.x * factor; \
		resultQuat->y = quat.y * factor; \
		resultQuat->z = quat.z * factor; \
	}

/**
 * @doc Multiply a quaternion by a given factor.
 */
QUAT_MULTIPLY_FACTOR(float)
QUAT_MULTIPLY_FACTOR(double)
QUAT_MULTIPLY_FACTOR(int8_t)
QUAT_MULTIPLY_FACTOR(int16_t)
QUAT_MULTIPLY_FACTOR(int32_t)
QUAT_MULTIPLY_FACTOR(int64_t)
QUAT_MULTIPLY_FACTOR(u_int8_t)
QUAT_MULTIPLY_FACTOR(u_int16_t)
QUAT_MULTIPLY_FACTOR(u_int32_t)
QUAT_MULTIPLY_FACTOR(u_int64_t)


/**
 * @doc Multiply two quaternions.
 */
static Quat quat_multiply(Quat q0, Quat q1)
{
	Quat resultQuat = {
		(q0.w * q1.w - q0.x * q1.x - q0.y * q1.y - q0.z * q1.z),
		(q0.w * q1.x + q0.x * q1.w + q0.y * q1.z - q0.z * q1.y),
		(q0.w * q1.y - q0.x * q1.z + q0.y * q1.w + q0.z * q1.x),
		(q0.w * q1.z + q0.x * q1.y - q0.y * q1.x + q0.z * q1.w)
	};

	return resultQuat;
} // end quat_multiply_quat



// ...


/**
 * @doc Convert radians to degrees.
 */
static double rad2deg(double radians)
{
	return radians * (180 / M_PI);
} // end rad2deg

/**
 * @doc Convert radians to degrees.
 */
static double deg2rad(double degrees)
{
	return degrees * (M_PI / 180);
} // end deg2rad


/* ----------------------------------------------------------------------------------------------------------------- */


/* ====================================================================================================================
 * Erlang API
 * ================================================================================================================= */
/* Exports: (% => leave implemented in Erlang; - => implement in C)
 * % quat_to_list/1
 * % list_to_quat/1
 * - add/2
 * - subtract/2
 * - multiply/2
 * - divide/2
 * - reorient/2
 * - scale_rotation/2
 * - norm/1
 * - length/1
 * - unit/1
 * - conjugate/1
 * - inverse/1
 * - reciprocal/1
 * - compose/2
 * - relative_to/2
 * - rotate/2
 * - from_axis_angle/2
 * - from_axis_angle/3
 * - from_body_rates/1
 * - from_body_rates/2
 * - from_euler/1
 * - from_euler/2
 * - rad2deg/1
 * - deg2rad/1
 * - is_zero/1
 */


#if 0 // For now, leave these implemented in Erlang; they're probably just as fast there, and not needed in C.
/**
 * @doc Convert from a quaternion to a list.
 */
// quat_to_list({W, X, Y, Z})
static ERL_NIF_TERM quat_to_list(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	[W, X, Y, Z]
} // end

/**
 * @doc Convert from a quaternion to a list.
 */
// list_to_quat([W, X, Y, Z])
static ERL_NIF_TERM list_to_quat(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	{W, X, Y, Z}
} // end
#endif

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Adds the two quaternions together.
 */
ERL_WRAPPER_Q_Q_TO_Q(quat_add)

/**
 * @doc Subtracts the second quaternion from the first.
 */
ERL_WRAPPER_Q_Q_TO_Q(quat_subtract)


#if 0 // This is what it looks like if we aren't doing macros like above...
static Quat add(const Quat q1, const Quat q2)
{
	Quat result;
	result.w = q1.w + q2.w;
	result.x = q1.x + q2.x;
	result.y = q1.y + q2.y;
	result.z = q1.z + q2.z;
	return result;
} // end add

// add({W1, X1, Y1, Z1}, {W2, X2, Y2, Z2})
static ERL_NIF_TERM add_erl(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
#ifdef SAFER_THAN_NECESSARY
	if(argc != 2) { return enif_make_badarg(env); }
#endif

	//{W1 + W2, X1 + X2, Y1 + Y2, Z1 + Z2}
	Quat inputQuat1;
	Quat inputQuat2;
	Quat resultQuat;

	if(!termToQuat(env, argv[0], &inputQuat1))
	{
		return enif_make_badarg(env);
	} // end if

	if(!termToQuat(env, argv[1], &inputQuat2))
	{
		return enif_make_badarg(env);
	} // end if

	return quatToTerm(env, resultQuat);
} // end add_erl
#endif

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * Helper for quat_multiply_erl
 */
static ERL_NIF_TERM _quat_multiply_factor_erl(ErlNifEnv* env, Quat quat, ERL_NIF_TERM factor)
{
	Quat resultQuat;

	double doubleFactor;
	ulong ulongFactor;
	long longFactor;
	ErlNifUInt64 uint64Factor;
	ErlNifSInt64 int64Factor;

	if(enif_get_double(env, factor, &doubleFactor))
		{ quat_multiply_factor_double(quat, doubleFactor, &resultQuat); }
	else if(enif_get_ulong(env, factor, &ulongFactor))
		{ quat_multiply_factor_u_int32_t(quat, ulongFactor, &resultQuat); }
	else if(enif_get_long(env, factor, &longFactor))
		{ quat_multiply_factor_int32_t(quat, longFactor, &resultQuat); }
	else if(enif_get_uint64(env, factor, &uint64Factor))
		{ quat_multiply_factor_u_int64_t(quat, uint64Factor, &resultQuat); }
	else if(enif_get_int64(env, factor, &int64Factor))
		{ quat_multiply_factor_int64_t(quat, int64Factor, &resultQuat); }

	return quatToTerm(env, resultQuat);
} // end _quat_multiply_factor_erl

/**
 * @doc Quaternion Multiplication
 */
// multiply(Factor, {W, X, Y, Z}) when is_integer(Factor); is_float(Factor)
static ERL_NIF_TERM quat_multiply_erl(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	Quat arg0Quat;
	int arg0IsQuat = (TRUE && termToQuat(env, argv[0], &arg0Quat));

	Quat arg1Quat;
	int arg1IsQuat = (TRUE && termToQuat(env, argv[1], &arg1Quat));

	if(arg0IsQuat && arg1IsQuat)
	{
		return quatToTerm(env, quat_multiply(arg0Quat, arg1Quat));
	}
	else if(!arg0IsQuat && arg1IsQuat)
	{
		return _quat_multiply_factor_erl(env, arg1Quat, argv[0]);
	}
	else if(arg0IsQuat && !arg1IsQuat)
	{
		return _quat_multiply_factor_erl(env, arg0Quat, argv[1]);
	}
	else
	{
		FAIL;
	} // end if
} // end quat_multiply_erl


#if 0
/**
 * @doc Scales the quaternion by the given factor.
 */
// divide({_, _, _, _}, 0)
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	{error, division_by_zero};

// divide({W, X, Y, Z}, Factor) when is_number(Factor)
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	{W / Factor, X / Factor, Y / Factor, Z / Factor}
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Reorient q1's axis of rotation by rotating it by q2, but leave q1's angle of rotation intact.
 */
// reorient({W, X, Y, Z}, {_, _, _, _}=Q2)
static ERL_NIF_TERM reorient(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	OriginalRotation = 2 * math:acos(W),
	Axis = rotate(vector:unit({X, Y, Z}), Q2),
	from_axis_angle(Axis, OriginalRotation)
} // end


/**
 * @doc Scale the rotation of the quaternion by the given factor. Note: This is not the same as multiplying.
 */
// scale_rotation(Factor, {W, X, Y, Z}) when is_integer(Factor); is_float(Factor)
static ERL_NIF_TERM scale_rotation(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	OriginalRotation = 2 * math:acos(W),
	Unit = vector:unit({X, Y, Z}),
	from_axis_angle(Unit, OriginalRotation * Factor)
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Returns the squared length of the quaternion. This is useful in some optimization cases, as it avoids a sqrt call.
 */
// squared_norm({W, X, Y, Z})
static ERL_NIF_TERM squared_norm(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	math:pow(W, 2) + math:pow(X, 2) + math:pow(Y, 2) + math:pow(Z, 2)
} // end


/**
 * @doc Returns the length of the quaternion.
 */
// norm({_, _, _, _} = Quat)
static ERL_NIF_TERM norm(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	math:sqrt(squared_norm(Quat))
} // end


/**
 * @doc Returns the length of the quaternion.
 */
// length(Quat)
static ERL_NIF_TERM length(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	norm(Quat)
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Returns a unit vector in the same direction as Quat.
 */
// unit({_, _, _, _} = Quat)
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	QLS = squared_norm(Quat),
	unit(QLS, Quat)
} // end


/**
 * @doc hidden
 */
// unit(0, {_, _, _, _} = Quat)
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	Quat;

/**
 * @doc hidden
 */
// unit(QLS, {_, _, _, _} = Quat)
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	Norm = abs(QLS - 1.0),
	case Norm < ?NORMALIZED_TOLERANCE of
		true ->
			Quat;
		_ ->
			divide(Quat, math:sqrt(QLS))
	end
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc
 */
// conjugate({W, X, Y, Z})
static ERL_NIF_TERM conjugate(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	{W, -X, -Y, -Z}
} // end


/**
 * @doc
 */
// inverse({_, _, _, _} = Quat)
static ERL_NIF_TERM inverse(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	divide(conjugate(Quat), norm(Quat))
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc
 */
// reciprocal({_, _, _, _} = Quat)
static ERL_NIF_TERM reciprocal(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	divide(conjugate(Quat), squared_norm(Quat))
} // end

/**
 * @doc Get the quaternion which results from composing the rotations represented by `first` and `second`.
 */
// compose({_, _, _, _} = First, {_, _, _, _} = Second)
static ERL_NIF_TERM compose(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	multiply(First, Second)
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Get the quaternion representing the orientation of `target` relative to `reference`.
 */
// relative_to({_, _, _, _} = Target, {_, _, _, _} = Reference)
static ERL_NIF_TERM relative_to(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	multiply(multiply(Reference, Target), conjugate(Reference))
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Rotates the vector by Rotation.
 */
// rotate({X, Y, Z}, {_, _, _, _} = Rotation)
static ERL_NIF_TERM rotate(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	{_, X1, Y1, Z1} = relative_to({0, X, Y, Z}, Rotation),
	{X1, Y1, Z1}
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Converts from and axis and angle (radians), to a quaternion.
 */
// from_axis_angle({_, _, _} = Axis, Angle) when is_number(Angle)
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	from_axis_angle(radians, Axis, Angle)
} // end


/**
 * @doc Converts from and axis and angle (radians), to a quaternion.
 */
// from_axis_angle(radians, {_, _, _} = Axis, Angle) when is_number(Angle)
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	ComplexFactor = math:sin(Angle / 2),
	{X, Y, Z} = vector:multiply(ComplexFactor, Axis),
	{math:cos(Angle / 2), X, Y, Z};

/**
 * @doc Converts from and axis and angle (degrees), to a quaternion.
 */
// from_axis_angle(degrees, Axis, Angle) when is_number(Angle)
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	DegAngle = deg2rad(Angle),
	from_axis_angle(radians, {_, _, _} = Axis, DegAngle)
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Converts from body rates (radians) to a quaternion.
 */
// from_body_rates({_, _, _} = Vec)
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	from_body_rates(radians, Vec)
} // end


/**
 * @doc Converts from body rates (radians) to a quaternion.
 */
// from_body_rates(radians, {X, Y, Z} = Vec)
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	case vector:is_zero(Vec) of
		true ->
			?IDENTITY;

		_ ->
			/* FIXME: THIS IS WRONG! */
			Vec1 = {Y, Z, X},
			Speed = vector:norm(Vec1),
			Axis = vector:divide(Vec1, Speed),
			from_axis_angle(Axis, Speed)
	end;

/**
 * @doc Converts from body rates (degrees) to a quaternion.
 */
// from_body_rates(degrees, {X, Y, Z})
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	from_body_rates(radians, {deg2rad(X), deg2rad(Y), deg2rad(Z)})
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Converts from a vector of euler angles (radians) to a quaternion.
 */
// from_euler({_, _, _} = Vec)
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	from_euler(radians, Vec)
} // end


/**
 * @doc Converts from a vector of euler angles (radians) to a quaternion.
 */
// from_euler(radians, {Yaw, Pitch, Roll})
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	HalfYaw = Yaw / 2,
	HalfPitch = Pitch / 2,
	HalfRoll = Roll / 2,

	{
		math:cos(HalfPitch) * math:cos(HalfRoll) * math:cos(HalfYaw) +
				math:sin(HalfPitch) * math:sin(HalfRoll) * math:sin(HalfYaw),

		math:sin(HalfPitch) * math:cos(HalfRoll) * math:cos(HalfYaw) -
				math:cos(HalfPitch) * math:sin(HalfRoll) * math:sin(HalfYaw),

		math:cos(HalfPitch) * math:sin(HalfRoll) * math:cos(HalfYaw) +
				math:sin(HalfPitch) * math:cos(HalfRoll) * math:sin(HalfYaw),

		math:cos(HalfPitch) * math:cos(HalfRoll) * math:sin(HalfYaw) +
				math:sin(HalfPitch) * math:sin(HalfRoll) * math:cos(HalfYaw)
	};

/**
 * @doc Converts from a vector of euler angles (degrees) to a quaternion.
 */
// from_euler(degrees, {Yaw, Pitch, Roll})
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	from_euler(radians, {deg2rad(Yaw), deg2rad(Pitch), deg2rad(Roll)})
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

#if 0 // For now, leave these implemented in Erlang; they're probably just as fast there.
/**
 * @doc Checks to see if this is a zero quaternion
 */
// is_zero({0, 0, 0, 0})
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	true;

// is_zero({_, _, _, _})
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, ERL_NIF_TERM argv[])
{
	false
} // end
#endif

#endif


/* ----------------------------------------------------------------------------------------------------------------- */
/* Declare Erlang exports */

static ErlNifFunc nif_funcs[] =
{
    {"add", 2, quat_add_erl},
    {"subtract", 2, quat_subtract_erl}
};

ERL_NIF_INIT(niftest, nif_funcs, NULL, NULL, NULL, NULL)
