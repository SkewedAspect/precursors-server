/**
 * @doc The Quat class
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <cmath>

#include <erl_nif.h>

#include "angles.h"

#include "Quat.h"


// --------------------------------------------------------------------------------------------------------------------
// Some useful constants

#define NORMALIZED_TOLERANCE 0.0000001


// --------------------------------------------------------------------------------------------------------------------

Quat::Quat() :
	w(0), x(0), y(0), z(0)
{
} // end Quat

Quat::Quat(double w, double x, double y, double z) :
	w(w), x(x), y(y), z(z)
{
} // end Quat


// ----------------------------------------------------------------------------------------------------------------
// Operations that produce new `Quat` instances

/// Returns a unit quaternion in the same direction as this Quat.
Quat Quat::unit() const
{
} // end unit

/// Returns a new Quat that is the conjugate of this one.
Quat Quat::conjugate() const
{
} // end conjugate

/// Returns a new Quat that is the inverse of this one.
Quat Quat::inverse() const
{
} // end inverse

/// Returns a new Quat that is the reciprocal of this one.
Quat Quat::reciprocal() const
{
} // end reciprocal

/// Returns a new Quat which results from composing this Quat's rotation with the given Quat.
Quat Quat::compose(const Quat& other) const
{
} // end compose

/// Get the Quat representing the orientation of this Quat relative to the given one.
Quat Quat::relativeTo(const Quat& other) const
{
} // end relativeTo


// Operator overloads (Quat <op> Quat)

Quat Quat::operator +(const Quat& other) const
{
} // end operator +

Quat Quat::operator -(const Quat& other) const
{
} // end operator -

Quat Quat::operator *(const Quat& other) const
{
} // end operator *


// Operator overloads (Quat <op> <other type>)

Quat Quat::operator *(const double& factor) const
{
} // end operator *

Quat Quat::operator *(const int32_t& factor) const
{
} // end operator *

Quat Quat::operator *(const u_int32_t& factor) const
{
} // end operator *

Quat Quat::operator *(const int64_t& factor) const
{
} // end operator *

Quat Quat::operator *(const u_int64_t& factor) const
{
} // end operator *

Quat Quat::operator /(const double& divisor) const
{
} // end operator /

Quat Quat::operator /(const int32_t& divisor) const
{
} // end operator /

Quat Quat::operator /(const u_int32_t& divisor) const
{
} // end operator /

Quat Quat::operator /(const int64_t& divisor) const
{
} // end operator /

Quat Quat::operator /(const u_int64_t& divisor) const
{
} // end operator /


// ----------------------------------------------------------------------------------------------------------------
// Operations that produce other values

/// Returns the squared length of the quaternion. This is useful in some optimization cases, as it avoids a sqrt call.
double Quat::squaredNorm() const
{
} // end squaredNorm

/// Returns the length of the quaternion.
double Quat::norm() const
{
} // end norm

/// Rotate the given vector by this Quat's rotation.
Vec Quat::rotate(const Vec& other) const
{
} // end rotate

/// Checks to see if this is a zero quaternion.
bool Quat::isZero() const
{
} // end isZero

/// Access components using Quat[idx]
double Quat::operator [](const size_t& idx) const throw(BadIndex<size_t>)
{
} // end operator []


// ----------------------------------------------------------------------------------------------------------------
// In-place modifications

/// Scale the rotation of the quaternion by the given factor.
///
/// NOTE: This is not the same as multiplying.
Quat& Quat::scaleRotation(const Quat& other)
{
} // end scaleRotation

/// Reorient this Quat's axis of rotation by rotating it by the given Quat, but leave this Quat's angle of rotation
/// intact.
Quat& Quat::reorient(const Quat& other)
{
} // end reorient

Quat& Quat::normalize()
{
} // end normalize

Quat& Quat::invert()
{
} // end invert


/// Converts from an axis and angle (degrees) to a quaternion.
Quat& Quat::fromAxisAngleDeg(const Vec& axis, double angleDeg)
{
	return fromAxisAngleRad(axis, deg2rad(angleDeg));
} // end fromAxisAngleDeg

/// Converts from an axis and angle (radians) to a quaternion.
Quat& Quat::fromAxisAngleRad(const Vec& axis, double angleRad)
{
	double halfAngle = angleRad / 2;
	double complexFactor = sin(halfAngle);

	w = cos(halfAngle);
	x = complexFactor * axis.x;
	y = complexFactor * axis.y;
	z = complexFactor * axis.z;

	return *this;
} // end fromAxisAngleRad

/// Converts from body rates (degrees) to a quaternion.
Quat& Quat::fromBodyRatesDeg(const Vec& bodyRatesDeg)
{
	return fromBodyRatesRad(Vec(deg2rad(bodyRatesDeg.x), deg2rad(bodyRatesDeg.y), deg2rad(bodyRatesDeg.z)));
} // end fromBodyRatesDeg

/// Converts from body rates (radians) to a quaternion.
Quat& Quat::fromBodyRatesRad(const Vec& bodyRatesRad)
{
	if(bodyRatesRad.isZero())
	{
		// The identity quaternion
		w = 1;
		x = 0;
		y = 0;
		z = 0;

		return *this;
	}
	else
	{
		//FIXME: According to the Erlang code, this is wrong! (not sure whether I entirely believe it)
		Vec axis(bodyRatesRad.y, bodyRatesRad.z, bodyRatesRad.x);

		double speed = axis.norm();
		axis /= speed;

		return fromAxisAngleRad(axis, speed);
	} // end if
} // end fromBodyRatesRad

/// Converts from a vector of euler angles ((yaw, pitch, roll), in degrees) to a quaternion.
Quat& Quat::fromEulerDeg(const Vec& anglesDeg)
{
	return fromEulerRad(Vec(deg2rad(anglesDeg.x), deg2rad(anglesDeg.y), deg2rad(anglesDeg.z)));
} // end fromEulerDeg

/// Converts from a vector of euler angles ((yaw, pitch, roll), in radians) to a quaternion.
Quat& Quat::fromEulerRad(const Vec& anglesRad)
{
	double halfYaw = anglesRad.x / 2;
	double halfPitch = anglesRad.y / 2;
	double halfRoll = anglesRad.z / 2;

	w = cos(halfPitch) * cos(halfRoll) * cos(halfYaw) + sin(halfPitch) * sin(halfRoll) * sin(halfYaw);
	x = sin(halfPitch) * cos(halfRoll) * cos(halfYaw) - cos(halfPitch) * sin(halfRoll) * sin(halfYaw);
	y = cos(halfPitch) * sin(halfRoll) * cos(halfYaw) + sin(halfPitch) * cos(halfRoll) * sin(halfYaw);
	z = cos(halfPitch) * cos(halfRoll) * sin(halfYaw) + sin(halfPitch) * sin(halfRoll) * cos(halfYaw);

	return *this;
} // end fromEulerRad


// Create a compound assignment operator overload that sets each component of the Quat to `this.coord OP other.coord`.
#define COMP_ASSIGN_OP_ZIP(OP, OTHER_TYPE) \
		Quat& Quat::operator OP##=(const OTHER_TYPE& other) \
		{ \
			w OP##= other.w; \
			x OP##= other.x; \
			y OP##= other.y; \
			z OP##= other.z; \
			return *this; \
		}

// Operator overloads (Quat <op> Quat)
COMP_ASSIGN_OP_ZIP(+, Quat)
COMP_ASSIGN_OP_ZIP(-, Quat)
COMP_ASSIGN_OP_ZIP(*, Quat)


// Create a compound assignment operator overload that sets each component of the Quat to `this.coord OP other`.
#define COMP_ASSIGN_OP_MAP(OP, OTHER_TYPE) \
		Quat& Quat::operator OP##=(const OTHER_TYPE& other) \
		{ \
			w OP##= other; \
			x OP##= other; \
			y OP##= other; \
			z OP##= other; \
			return *this; \
		}

// Operator overloads (Quat <op>= <other type>)
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
