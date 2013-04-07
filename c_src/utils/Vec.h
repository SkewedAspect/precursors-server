/**
 * @doc The Vec class
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#ifndef __VEC__
#define __VEC__

#include "exceptions.h"


#ifdef __GNUC__
#	define FLATTEN __attribute__((flatten))
#else
#	define FLATTEN
#endif


/**
 * A vector. (x, y, z)
 */
class Vec
{
public:
	// Constructors
	Vec();

	Vec(double x, double y, double z);


	// ----------------------------------------------------------------------------------------------------------------
	// Operations that produce new `Vec` instances

	/// Perform a cross product with the given Vec.
	Vec cross(const Vec& other) const;

	/// Returns a unit vector in the same direction as this Vec.
	Vec unit() const;

	/// Gets the Yaw and Pitch required to point in the same direction as this Vec.
	Vec hprAlong() const;

	// Operator overloads (Vec <op> Vec)
	Vec operator +(const Vec& other) const;
	Vec operator -(const Vec& other) const;

	// Operator overloads (Vec <op> <other type>)
	Vec operator *(const double& factor) const;
	Vec operator *(const int32_t& factor) const;
	Vec operator *(const u_int32_t& factor) const;
	Vec operator *(const int64_t& factor) const;
	Vec operator *(const u_int64_t& factor) const;
	Vec operator /(const double& divisor) const;
	Vec operator /(const int32_t& divisor) const;
	Vec operator /(const u_int32_t& divisor) const;
	Vec operator /(const int64_t& divisor) const;
	Vec operator /(const u_int64_t& divisor) const;


	// ----------------------------------------------------------------------------------------------------------------
	// Operations that produce other values

	/// Perform dot product.
	double dot(const Vec& other) const;

	/// Returns the squared length of the vector. This is useful in some optimization cases, as it avoids a sqrt call.
	double squaredNorm() const;

	/// Returns the length of the vector.
	double norm() const;

	/// Returns the length of the vector.
	inline double FLATTEN length() const { return norm(); }

	/// Checks to see if this is a zero vector.
	bool is_zero() const;

	/// Access components using Vec[idx]
	double operator [](const size_t& idx) const throw(BadIndex<size_t>);


	// ----------------------------------------------------------------------------------------------------------------
	// In-place modifications

	Vec& normalize();

	// Operator overloads (Vec <op>= Vec)
	Vec& operator +=(const Vec& other);
	Vec& operator -=(const Vec& other);

	// Operator overloads (Vec <op>= <other type>)
	Vec& operator *=(const double& factor);
	Vec& operator *=(const int32_t& factor);
	Vec& operator *=(const u_int32_t& factor);
	Vec& operator *=(const int64_t& factor);
	Vec& operator *=(const u_int64_t& factor);
	Vec& operator /=(const double& divisor);
	Vec& operator /=(const int32_t& divisor);
	Vec& operator /=(const u_int32_t& divisor);
	Vec& operator /=(const int64_t& divisor);
	Vec& operator /=(const u_int64_t& divisor);


	// ----------------------------------------------------------------------------------------------------------------
	// Properties

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
