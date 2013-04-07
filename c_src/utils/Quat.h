/**
 * @doc The Quat class
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#ifndef __QUAT__
#define __QUAT__

#include "exceptions.h"

#include "Vec.h"


#ifdef __GNUC__
#	define FLATTEN __attribute__((flatten))
#else
#	define FLATTEN
#endif


/**
 * A quaternion. (w + xi + yj + zk)
 */
class Quat
{
public:
	Quat();

	Quat(double w, double x, double y, double z);


	// ----------------------------------------------------------------------------------------------------------------
	// Operations that produce new `Quat` instances

	/// Returns a unit quaternion in the same direction as this Quat.
	Quat unit() const;

	/// Returns a new Quat that is the conjugate of this one.
	Quat conjugate() const;

	/// Returns a new Quat that is the inverse of this one.
	Quat inverse() const;

	/// Returns a new Quat that is the reciprocal of this one.
	Quat reciprocal() const;

	/// Returns a new Quat which results from composing this Quat's rotation with the given Quat.
	Quat compose(const Quat& other) const;

	/// Get the Quat representing the orientation of this Quat relative to the given one.
	Quat relativeTo(const Quat& other) const;

	// Operator overloads (Quat <op> Quat)
	Quat operator +(const Quat& other) const;
	Quat operator -(const Quat& other) const;
	Quat operator *(const Quat& other) const;

	// Operator overloads (Quat <op> <other type>)
	Quat operator *(const double& factor) const;
	Quat operator *(const int32_t& factor) const;
	Quat operator *(const u_int32_t& factor) const;
	Quat operator *(const int64_t& factor) const;
	Quat operator *(const u_int64_t& factor) const;
	Quat operator /(const double& divisor) const;
	Quat operator /(const int32_t& divisor) const;
	Quat operator /(const u_int32_t& divisor) const;
	Quat operator /(const int64_t& divisor) const;
	Quat operator /(const u_int64_t& divisor) const;


	// ----------------------------------------------------------------------------------------------------------------
	// Operations that produce other values

	/// Returns the squared length of the quaternion. This is useful in some optimization cases, as it avoids a sqrt call.
	double squaredNorm() const;

	/// Returns the length of the quaternion.
	double norm() const;

	/// Returns the length of the quaternion.
	inline double FLATTEN length() const { return norm(); }

	/// Rotate the given vector by this Quat's rotation.
	Vec rotate(const Vec& other) const;

	/// Checks to see if this is a zero quaternion.
	bool isZero() const;

	/// Access components using Quat[idx]
	double operator [](const size_t& idx) const throw(BadIndex<size_t>);


	// ----------------------------------------------------------------------------------------------------------------
	// In-place modifications

	/// Scale the rotation of the quaternion by the given factor.
	///
	/// NOTE: This is not the same as multiplying.
	Quat& scaleRotation(double factor);

	/// Reorient this Quat's axis of rotation by rotating it by the given Quat, but leave this Quat's angle of rotation
	/// intact.
	Quat& reorient(const Quat& other);

	Quat& normalize();
	Quat& invert();

	/// Converts from an axis and angle (degrees) to a quaternion.
	Quat& fromAxisAngleDeg(const Vec& axis, double angleDeg);

	/// Converts from an axis and angle (radians) to a quaternion.
	Quat& fromAxisAngleRad(const Vec& axis, double angleRad);

	/// Converts from body rates (degrees) to a quaternion.
	Quat& fromBodyRatesDeg(const Vec& bodyRatesDeg);

	/// Converts from body rates (radians) to a quaternion.
	Quat& fromBodyRatesRad(const Vec& bodyRatesRad);

	/// Converts from a vector of euler angles ((yaw, pitch, roll), in degrees) to a quaternion.
	Quat& fromEulerDeg(const Vec& anglesDeg);

	/// Converts from a vector of euler angles ((yaw, pitch, roll), in radians) to a quaternion.
	Quat& fromEulerRad(const Vec& anglesRad);

	// Operator overloads (Quat <op>= Quat)
	Quat& operator +=(const Quat& other);
	Quat& operator -=(const Quat& other);
	Quat& operator *=(const Quat& other);

	// Operator overloads (Quat <op>= <other type>)
	Quat& operator *=(const double& factor);
	Quat& operator *=(const int32_t& factor);
	Quat& operator *=(const u_int32_t& factor);
	Quat& operator *=(const int64_t& factor);
	Quat& operator *=(const u_int64_t& factor);
	Quat& operator /=(const double& divisor);
	Quat& operator /=(const int32_t& divisor);
	Quat& operator /=(const u_int32_t& divisor);
	Quat& operator /=(const int64_t& divisor);
	Quat& operator /=(const u_int64_t& divisor);


	// ----------------------------------------------------------------------------------------------------------------
	// Properties

	// Components
	double w;
	double x;
	double y;
	double z;
}; // end Quat


// Operator overloads (<other type> <op> Quat)
Quat operator *(const double& factor, const Quat& quat);
Quat operator *(const int32_t& factor, const Quat& quat);
Quat operator *(const u_int32_t& factor, const Quat& quat);
Quat operator *(const int64_t& factor, const Quat& quat);
Quat operator *(const u_int64_t& factor, const Quat& quat);

#endif // __QUAT__
