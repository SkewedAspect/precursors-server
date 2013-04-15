/**
 * @doc Helper functions for working with angles.
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include <cmath>

#include "angles.h"


/**
 * @doc Convert radians to degrees.
 */
double rad2deg(double radians)
{
	return radians * (180 / M_PI);
} // end rad2deg

/**
 * @doc Convert radians to degrees.
 */
double deg2rad(double degrees)
{
	return degrees * (M_PI / 180);
} // end deg2rad
