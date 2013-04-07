/**
 * @doc The Vec class
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <cmath>

#include <erl_nif.h>

#include "angles.h"

#include "Vec.h"


/* ----------------------------------------------------------------------------------------------------------------- */
/* Some useful constants */

#define NORMALIZED_TOLERANCE 0.0000001


// --------------------------------------------------------------------------------------------------------------------

Vec::Vec() :
	x(0), y(0), z(0)
{
} // end Vec

Vec::Vec(double x, double y, double z) :
	x(x), y(y), z(z)
{
} // end Vec


// --------------------------------------------------------------------------------------------------------------------
// Operations that produce new `Vec` instances

/// Perform cross product.
Vec Vec::cross(const Vec& other) const
{
	return Vec(
			y * other.z - z * other.y,
			-x * other.z + z * other.x,
			x * other.y - y * other.x
			);
} // end cross

/// Returns a unit vector in the same direction as Vec.
Vec Vec::unit() const
{
	return Vec(*this).normalize();
} // end unit

/// Gets the Yaw and Pitch required to point in the direction of the given vector.
Vec Vec::hprAlong() const
{
	Vec tempVec = unit();
	double yaw = -atan2(tempVec.x, tempVec.y);
	double pitch = atan2(tempVec.z, sqrt(pow(tempVec.x, 2) + pow(tempVec.y, 2)));

	return Vec(rad2deg(yaw), rad2deg(pitch), 0);
} // end hpr_to


// As stated at http://courses.cms.caltech.edu/cs11/material/cpp/donnie/cpp-ops.html,
//     "Define your binary arithmetic operators using your compound assignment operators."
#define BINARY_OP(OP, OTHER_TYPE) \
		Vec Vec::operator OP(const OTHER_TYPE& other) const \
			{ return Vec(*this) OP##= other; }

// Operator overloads (Vec <op> Vec)
BINARY_OP(+, Vec)
BINARY_OP(-, Vec)

// Operator overloads (Vec <op> <other type>)
BINARY_OP(*, double)
BINARY_OP(*, int32_t)
BINARY_OP(*, u_int32_t)
BINARY_OP(*, int64_t)
BINARY_OP(*, u_int64_t)
BINARY_OP(/, double)
BINARY_OP(/, int32_t)
BINARY_OP(/, u_int32_t)
BINARY_OP(/, int64_t)
BINARY_OP(/, u_int64_t)


// --------------------------------------------------------------------------------------------------------------------
// Operations that produce other values

/// Perform dot product.
double Vec::dot(const Vec& other) const
{
	return x * other.x + y * other.y + z * other.z;
} // end dot

/// Returns the squared length of the vector. This is useful in some optimization cases, as it avoids a sqrt call.
double Vec::squaredNorm() const
{
	// Yes, these two are equivalent.
	//return dot(*this);
	return x * x + y * y + z * z;
} // end squared_norm

/// Returns the length of the vector.
double Vec::norm() const
{
	return sqrt(squaredNorm());
} // end norm

/// Checks to see if this is a zero vector.
bool Vec::isZero() const
{
	return fabs(x) < NORMALIZED_TOLERANCE && fabs(y) < NORMALIZED_TOLERANCE && fabs(z) < NORMALIZED_TOLERANCE;
} // end isZero

/// Access components using Vec[idx]
double Vec::operator [](const size_t& idx) const throw(BadIndex<size_t>)
{
	if(idx > 2 || idx < 0)
	{
		throw BadIndex<size_t>(idx);
	} // end if

	return *(&x + idx);

	/* NOTE: This is equivalent to:
	switch(idx)
	{
		case 0:
			return x;
		case 1:
			return y;
		case 2:
			return z;
		default:
			throw BadIndex<size_t>(idx);
	} // end switch
	*/
} // end operator []


// --------------------------------------------------------------------------------------------------------------------
// In-place modifications

/// Changes the length of this vector so that it's a unit vector.
Vec& Vec::normalize()
{
	double sqrNorm = squaredNorm();

	// Don't renormalize if we're already less than our tolerance away from being a unit vector.
	if(fabs(sqrNorm - 1) < NORMALIZED_TOLERANCE)
	{
		return *this;
	} // end if

	// Also don't renormalize if we're already less than our tolerance away from being a zero vector.
	if(sqrNorm < NORMALIZED_TOLERANCE)
	{
		return *this;
	} // end if

	return *this /= sqrt(sqrNorm);
} // end normalize


// Create a compound assignment operator overload that sets each component of the Vec to `this.coord OP other.coord`.
#define COMP_ASSIGN_OP_ZIP(OP, OTHER_TYPE) \
		Vec& Vec::operator OP##=(const OTHER_TYPE& other) \
		{ \
			x OP##= other.x; \
			y OP##= other.y; \
			z OP##= other.z; \
			return *this; \
		}

// Operator overloads (Vec <op> Vec)
COMP_ASSIGN_OP_ZIP(+, Vec)
COMP_ASSIGN_OP_ZIP(-, Vec)


// Create a compound assignment operator overload that sets each component of the Vec to `this.coord OP other`.
#define COMP_ASSIGN_OP_MAP(OP, OTHER_TYPE) \
		Vec& Vec::operator OP##=(const OTHER_TYPE& other) \
		{ \
			x OP##= other; \
			y OP##= other; \
			z OP##= other; \
			return *this; \
		}

// Operator overloads (Vec <op>= <other type>)
COMP_ASSIGN_OP_MAP(*, double)
COMP_ASSIGN_OP_MAP(*, int32_t)
COMP_ASSIGN_OP_MAP(*, u_int32_t)
COMP_ASSIGN_OP_MAP(*, int64_t)
COMP_ASSIGN_OP_MAP(*, u_int64_t)
COMP_ASSIGN_OP_MAP(/, double)
COMP_ASSIGN_OP_MAP(/, int32_t)
COMP_ASSIGN_OP_MAP(/, u_int32_t)
COMP_ASSIGN_OP_MAP(/, int64_t)
COMP_ASSIGN_OP_MAP(/, u_int64_t)
