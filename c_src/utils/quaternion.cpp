/**
 * @doc Quaternion module - simplifies working with and perfoming math on quaternions.
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <cmath>
#include <cstdio>

#include <erl_nif.h>

#include "quaternion.h"
#include "exceptions.h"


static bool getNIFDouble(ErlNifEnv* env, const ERL_NIF_TERM term, double* target);


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
 * Define an operator overload that computes a new Quat where each coordinate equals `this.coord OP other.coord`.
 */
#define PAIRWISE_OP__TO_QUAT(OP) \
	Quat Quat::operator OP(const Quat& other) const \
	{ \
		Quat result; \
		result.w = w OP other.w; \
		result.x = x OP other.x; \
		result.y = y OP other.y; \
		result.z = z OP other.z; \
		return result; \
	}


/* ====================================================================================================================
 * C++-only API
 * ================================================================================================================= */

Quat::Quat() :
	w(0), x(0), y(0), z(0)
{
} // end Quat

Quat::Quat(double w, double x, double y, double z) :
	w(w), x(x), y(y), z(z)
{
} // end Quat

Quat::Quat(ErlNifEnv* env, const ERL_NIF_TERM term)
{
	readFromTerm(env, term);
} // end Quat

bool Quat::readFromTerm(ErlNifEnv* env, const ERL_NIF_TERM term)
{
	int arity;
	const ERL_NIF_TERM* array = NULL;

	if(!enif_get_tuple(env, term, &arity, &array))
	{
		printf("Quat::readFromTerm: Couldn't get tuple!\n");
		return false;
	} // end if

	if(arity != 4)
	{
		printf("Quat::readFromTerm: Bad arity!\n");
		return false;
	} // end if

	if(!getNIFDouble(env, array[0], &w)
			|| !getNIFDouble(env, array[1], &x)
			|| !getNIFDouble(env, array[2], &y)
			|| !getNIFDouble(env, array[3], &z))
	{
		printf("Quat::readFromTerm: Failed to get components from the Erlang term! (current value: {%f, %f, %f, %f})\n",
				w, x, y, z);
		return false;
	} // end if

	return true;
} // readFromTerm

ERL_NIF_TERM Quat::toTerm(ErlNifEnv* env)
{
	return enif_make_tuple4(env,
			enif_make_double(env, w),
			enif_make_double(env, x),
			enif_make_double(env, y),
			enif_make_double(env, z)
			);
} // end toTerm

/**
 * Multiply this Quat by whatever's stored in the given Erlang term, if possible.
 */
//static ERL_NIF_TERM _quat_multiply_factor_erl(ErlNifEnv* env, Quat quat, const ERL_NIF_TERM factor)
Quat Quat::multiply(ErlNifEnv* env, const ERL_NIF_TERM other) const
{
	Quat resultQuat;

	Quat quatOther;
	double doubleOther;
	ulong ulongOther;
	long longOther;
	ErlNifUInt64 uint64Other;
	ErlNifSInt64 int64Other;

	if(quatOther.readFromTerm(env, other))
		{ resultQuat = *this * quatOther; }
	else if(enif_get_double(env, other, &doubleOther))
		{ resultQuat = *this * doubleOther; }
	else if(enif_get_ulong(env, other, &ulongOther))
		{ resultQuat = *this * ulongOther; }
	else if(enif_get_long(env, other, &longOther))
		{ resultQuat = *this * longOther; }
	else if(enif_get_uint64(env, other, &uint64Other))
		{ resultQuat = *this * uint64Other; }
	else if(enif_get_int64(env, other, &int64Other))
		{ resultQuat = *this * int64Other; }

	return resultQuat;
} // end multiply


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

// Operator overloads (Quat <op> Quat)
//
/// Adds the two quaternions together.
PAIRWISE_OP__TO_QUAT(+)

/// Subtracts the second quaternion from the first.
PAIRWISE_OP__TO_QUAT(-)

/// Multiply two quaternions.
Quat Quat::operator *(const Quat& other) const
{
	Quat resultQuat(
		(w * other.w - x * other.x - y * other.y - z * other.z),
		(w * other.x + x * other.w + y * other.z - z * other.y),
		(w * other.y - x * other.z + y * other.w + z * other.x),
		(w * other.z + x * other.y - y * other.x + z * other.w)
	);

	return resultQuat;
} // end operator *

// Operator overloads (Quat <op> <other type>)
#define QUAT_MULTIPLY_FACTOR(FACTOR_TYPE) \
	Quat Quat::operator *(const FACTOR_TYPE factor) const \
	{ \
		return Quat(w * factor, x * factor, y * factor, z * factor); \
	}
QUAT_MULTIPLY_FACTOR(double)
QUAT_MULTIPLY_FACTOR(int32_t)
QUAT_MULTIPLY_FACTOR(int64_t)
QUAT_MULTIPLY_FACTOR(u_int32_t)
QUAT_MULTIPLY_FACTOR(u_int64_t)


// Access components using Quat[idx]
double Quat::operator [](const size_t idx) const
{
	switch(idx)
	{
		case 0:
			return w;
		case 1:
			return x;
		case 2:
			return y;
		case 3:
			return z;
		default:
			throw BadIndex<size_t>(idx);
	} // end switch
} // end operator []

// Operator overloads (<other type> <op> Quat)
static Quat operator *(const double factor, const Quat& quat) { return quat * factor; }
static Quat operator *(const int32_t factor, const Quat& quat) { return quat * factor; }
static Quat operator *(const u_int32_t factor, const Quat& quat) { return quat * factor; }
static Quat operator *(const int64_t factor, const Quat& quat) { return quat * factor; }
static Quat operator *(const u_int64_t factor, const Quat& quat) { return quat * factor; }


/* ====================================================================================================================
 * Erlang API
 * ================================================================================================================= */

/**
 * Create a NIF which wraps a C++ method with the signature: Quat Quat::function(Quat)
 */
#define ERL_WRAPPER_Q_TO_Q(NAME) \
	static ERL_NIF_TERM quat_##NAME##_erl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
	{ \
		Quat inputQuat1; \
		Quat inputQuat2; \
		Quat resultQuat; \
		\
		CHECK_ARGC(2); \
		FAIL_IF(!inputQuat1.readFromTerm(env, argv[0])) \
		FAIL_IF(!inputQuat2.readFromTerm(env, argv[1])) \
		\
		resultQuat = inputQuat1.NAME(inputQuat2); \
		\
		return resultQuat.toTerm(env); \
	}


/**
 * Create a NIF which wraps a C++ operator with the signature: Quat Quat::operator OP(Quat)
 */
#define ERL_WRAPPER_OP_Q_TO_Q(NAME, OP) \
	static ERL_NIF_TERM quat_##NAME##_erl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
	{ \
		Quat inputQuat1; \
		Quat inputQuat2; \
		\
		CHECK_ARGC(2); \
		FAIL_IF(!inputQuat1.readFromTerm(env, argv[0])) \
		FAIL_IF(!inputQuat2.readFromTerm(env, argv[1])) \
		\
		return (inputQuat1 OP inputQuat2).toTerm(env); \
	}


#if 0 // For now, leave these implemented in Erlang; they're probably just as fast there, and not needed in C.
/**
 * @doc Convert from a quaternion to a list.
 */
// quat_to_list({W, X, Y, Z})
static ERL_NIF_TERM quat_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	[W, X, Y, Z]
} // end

/**
 * @doc Convert from a quaternion to a list.
 */
// list_to_quat([W, X, Y, Z])
static ERL_NIF_TERM list_to_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	{W, X, Y, Z}
} // end
#endif

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Adds the two quaternions together.
 */
ERL_WRAPPER_OP_Q_TO_Q(add, +)

/**
 * @doc Subtracts the second quaternion from the first.
 */
ERL_WRAPPER_OP_Q_TO_Q(subtract, -)


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
static ERL_NIF_TERM add_erl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef SAFER_THAN_NECESSARY
	if(argc != 2) { return enif_make_badarg(env); }
#endif

	//{W1 + W2, X1 + X2, Y1 + Y2, Z1 + Z2}
	Quat inputQuat1;
	Quat inputQuat2;
	Quat resultQuat;

	if(!Quat::readFromTerm(env, argv[0], &inputQuat1))
	{
		return enif_make_badarg(env);
	} // end if

	if(!Quat::readFromTerm(env, argv[1], &inputQuat2))
	{
		return enif_make_badarg(env);
	} // end if

	return resultQuat.toTerm(env);
} // end add_erl
#endif

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Quaternion Multiplication
 */
// multiply(Factor, {W, X, Y, Z}) when is_integer(Factor); is_float(Factor)
static ERL_NIF_TERM quat_multiply_erl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	Quat argQuat;
	if(argQuat.readFromTerm(env, argv[0]))
	{
		return argQuat.multiply(env, argv[1]).toTerm(env);
	}
	else if(argQuat.readFromTerm(env, argv[1]))
	{
		return argQuat.multiply(env, argv[0]).toTerm(env);
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
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	{error, division_by_zero};

// divide({W, X, Y, Z}, Factor) when is_number(Factor)
static ERL_NIF_TERM divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	{W / Factor, X / Factor, Y / Factor, Z / Factor}
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Reorient q1's axis of rotation by rotating it by q2, but leave q1's angle of rotation intact.
 */
// reorient({W, X, Y, Z}, {_, _, _, _}=Q2)
static ERL_NIF_TERM reorient(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	OriginalRotation = 2 * math:acos(W),
	Axis = rotate(vector:unit({X, Y, Z}), Q2),
	from_axis_angle(Axis, OriginalRotation)
} // end


/**
 * @doc Scale the rotation of the quaternion by the given factor. Note: This is not the same as multiplying.
 */
// scale_rotation(Factor, {W, X, Y, Z}) when is_integer(Factor); is_float(Factor)
static ERL_NIF_TERM scale_rotation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM squared_norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	math:pow(W, 2) + math:pow(X, 2) + math:pow(Y, 2) + math:pow(Z, 2)
} // end


/**
 * @doc Returns the length of the quaternion.
 */
// norm({_, _, _, _} = Quat)
static ERL_NIF_TERM norm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	math:sqrt(squared_norm(Quat))
} // end


/**
 * @doc Returns the length of the quaternion.
 */
// length(Quat)
static ERL_NIF_TERM length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	norm(Quat)
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Returns a unit vector in the same direction as Quat.
 */
// unit({_, _, _, _} = Quat)
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	QLS = squared_norm(Quat),
	unit(QLS, Quat)
} // end


/**
 * @doc hidden
 */
// unit(0, {_, _, _, _} = Quat)
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	Quat;

/**
 * @doc hidden
 */
// unit(QLS, {_, _, _, _} = Quat)
static ERL_NIF_TERM unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM conjugate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	{W, -X, -Y, -Z}
} // end


/**
 * @doc
 */
// inverse({_, _, _, _} = Quat)
static ERL_NIF_TERM inverse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	divide(conjugate(Quat), norm(Quat))
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc
 */
// reciprocal({_, _, _, _} = Quat)
static ERL_NIF_TERM reciprocal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	divide(conjugate(Quat), squared_norm(Quat))
} // end

/**
 * @doc Get the quaternion which results from composing the rotations represented by `first` and `second`.
 */
// compose({_, _, _, _} = First, {_, _, _, _} = Second)
static ERL_NIF_TERM compose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	multiply(First, Second)
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Get the quaternion representing the orientation of `target` relative to `reference`.
 */
// relative_to({_, _, _, _} = Target, {_, _, _, _} = Reference)
static ERL_NIF_TERM relative_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	multiply(multiply(Reference, Target), conjugate(Reference))
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Rotates the vector by Rotation.
 */
// rotate({X, Y, Z}, {_, _, _, _} = Rotation)
static ERL_NIF_TERM rotate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	{_, X1, Y1, Z1} = relative_to({0, X, Y, Z}, Rotation),
	{X1, Y1, Z1}
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Converts from and axis and angle (radians), to a quaternion.
 */
// from_axis_angle({_, _, _} = Axis, Angle) when is_number(Angle)
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	from_axis_angle(radians, Axis, Angle)
} // end


/**
 * @doc Converts from and axis and angle (radians), to a quaternion.
 */
// from_axis_angle(radians, {_, _, _} = Axis, Angle) when is_number(Angle)
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ComplexFactor = math:sin(Angle / 2),
	{X, Y, Z} = vector:multiply(ComplexFactor, Axis),
	{math:cos(Angle / 2), X, Y, Z};

/**
 * @doc Converts from and axis and angle (degrees), to a quaternion.
 */
// from_axis_angle(degrees, Axis, Angle) when is_number(Angle)
static ERL_NIF_TERM from_axis_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	DegAngle = deg2rad(Angle),
	from_axis_angle(radians, {_, _, _} = Axis, DegAngle)
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Converts from body rates (radians) to a quaternion.
 */
// from_body_rates({_, _, _} = Vec)
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	from_body_rates(radians, Vec)
} // end


/**
 * @doc Converts from body rates (radians) to a quaternion.
 */
// from_body_rates(radians, {X, Y, Z} = Vec)
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM from_body_rates(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	from_body_rates(radians, {deg2rad(X), deg2rad(Y), deg2rad(Z)})
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

/**
 * @doc Converts from a vector of euler angles (radians) to a quaternion.
 */
// from_euler({_, _, _} = Vec)
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	from_euler(radians, Vec)
} // end


/**
 * @doc Converts from a vector of euler angles (radians) to a quaternion.
 */
// from_euler(radians, {Yaw, Pitch, Roll})
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
static ERL_NIF_TERM from_euler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	from_euler(radians, {deg2rad(Yaw), deg2rad(Pitch), deg2rad(Roll)})
} // end

/* ----------------------------------------------------------------------------------------------------------------- */

#if 0 // For now, leave these implemented in Erlang; they're probably just as fast there.
/**
 * @doc Checks to see if this is a zero quaternion
 */
// is_zero({0, 0, 0, 0})
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	true;

// is_zero({_, _, _, _})
static ERL_NIF_TERM is_zero(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    {"subtract", 2, quat_subtract_erl},
    {"multiply", 2, quat_multiply_erl}
};

ERL_NIF_INIT(quaternion, nif_funcs, NULL, NULL, NULL, NULL)


/* ====================================================================================================================
 * Internal Helpers
 * ================================================================================================================= */

static bool getNIFDouble(ErlNifEnv* env, const ERL_NIF_TERM term, double* target)
{
	ulong ulongOther;
	long longOther;
	ErlNifUInt64 uint64Other;
	ErlNifSInt64 int64Other;

	if(enif_get_double(env, term, target))
		{ return true; }
	else if(enif_get_ulong(env, term, &ulongOther))
		{ *target = ulongOther; return true; }
	else if(enif_get_long(env, term, &longOther))
		{ *target = longOther; return true; }
	else if(enif_get_uint64(env, term, &uint64Other))
		{ *target = uint64Other; return true; }
	else if(enif_get_int64(env, term, &int64Other))
		{ *target = int64Other; return true; }

	return false;
} // end getNIFDouble
