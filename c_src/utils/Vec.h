/**
 * @doc The Vec class
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#ifndef __VEC__
#define __VEC__


/**
 * A vector. (x, y, z)
 */
class Vec
{
public:
	Vec();

	Vec(double x, double y, double z);

	Vec(ErlNifEnv* env, const ERL_NIF_TERM);

	// Operations with Erlang terms
	Vec multiply(ErlNifEnv* env, const ERL_NIF_TERM other) const;

	// Operator overloads (Vec <op> Vec)
	Vec operator +(const Vec& other) const;
	Vec operator -(const Vec& other) const;

	// Operator overloads (Vec <op> <other type>)
	Vec operator *(const double factor) const;
	Vec operator *(const int32_t factor) const;
	Vec operator *(const u_int32_t factor) const;
	Vec operator *(const int64_t factor) const;
	Vec operator *(const u_int64_t factor) const;
	Vec operator /(const double divisor) const;
	Vec operator /(const int32_t divisor) const;
	Vec operator /(const u_int32_t divisor) const;
	Vec operator /(const int64_t divisor) const;
	Vec operator /(const u_int64_t divisor) const;

	// Access components using Vec[idx]
	Vec operator [](const size_t idx) const;

	// Components
	double x;
	double y;
	double z;
}; // end Vec


// Operator overloads (<other type> <op> Quat)
Vec operator *(const double factor, const Vec& vec);
Vec operator *(const int32_t factor, const Vec& vec);
Vec operator *(const u_int32_t factor, const Vec& vec);
Vec operator *(const int64_t factor, const Vec& vec);
Vec operator *(const u_int64_t factor, const Vec& vec);


#endif // __VEC__
