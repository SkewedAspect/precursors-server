#ifndef __QUATERNION__
#define __QUATERNION__

/**
 * A quaternion. (w + xi + yj + zk)
 */
class Quat
{
public:
	Quat();

	Quat(double w, double x, double y, double z);

	Quat(ErlNifEnv* env, const ERL_NIF_TERM);

	// Conversion
	bool readFromTerm(ErlNifEnv* env, const ERL_NIF_TERM term);

	ERL_NIF_TERM toTerm(ErlNifEnv* env);

	// Operations with Erlang terms
	Quat multiply(ErlNifEnv* env, const ERL_NIF_TERM other) const;

	// Operator overloads (Quat <op> Quat)
	Quat operator +(const Quat& rhs) const;
	Quat operator -(const Quat& rhs) const;
	Quat operator *(const Quat& rhs) const;

	// Operator overloads (Quat <op> <other type>)
	Quat operator *(const double rhs) const;
	Quat operator *(const int32_t factor) const;
	Quat operator *(const u_int32_t factor) const;
	Quat operator *(const int64_t factor) const;
	Quat operator *(const u_int64_t factor) const;

	// Access components using Quat[idx]
	double operator [](const size_t idx) const;

	// Components
	double w;
	double x;
	double y;
	double z;
}; // end Quat

// Operator overloads (<other type> <op> Quat)
Quat operator *(const double factor, const Quat& quat);
Quat operator *(const int32_t factor, const Quat& quat);
Quat operator *(const u_int32_t factor, const Quat& quat);
Quat operator *(const int64_t factor, const Quat& quat);
Quat operator *(const u_int64_t factor, const Quat& quat);

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
 * % % quat_to_list/1
 * % % list_to_quat/1
 * + + add/2
 * + + subtract/2
 * + + multiply/2
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
 * + ? rad2deg/1
 * + ? deg2rad/1
 * ! ? is_zero/1
 */

#endif // __QUATERNION__
