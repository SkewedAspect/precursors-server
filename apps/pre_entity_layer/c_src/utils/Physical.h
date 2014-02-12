/**
 * @doc The Physical class
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#ifndef __PHYSICAL__
#define __PHYSICAL__

#include <erl_driver.h>

#include "Vec.h"
#include "Quat.h"


/**
 * A physical object.
 */
class Physical
{
public:
	// Constructor
	Physical();


	// ----------------------------------------------------------------------------------------------------------------
	// In-place modifications

	void setMass(float mass);

	void setInertiaTensor(float inertiaTensor);

	/**
	 * Simulate physical movement of the 'physical' object represented by `InitialPhysical`, over the time since that
	 * object's 'lastUpdate' timestamp, or over the given `timeDelta` (in microseconds).
	 */
	void simulate(unsigned long timeDelta_us = (unsigned long) -1);


	// ----------------------------------------------------------------------------------------------------------------
	// Operations that produce other values

	unsigned long timeSinceLastUpdate() const;


	// ----------------------------------------------------------------------------------------------------------------
	// Properties

	// Input-only values
	Vec forceAbsolute;
	Vec forceRelative;
	Vec torqueAbsolute;
	Vec torqueRelative;

	struct DynamicState
	{
		DynamicState();

		// Updated values (assume these change every frame)
		Vec position;
		Vec linearMomentum;
		Quat orientation;
		Vec angularMomentum;

		// Purely calculated values (DON'T try to change these externally)
		Vec linearVelocity;
		Vec angularVelocity;
		Quat spin;
	}; // end DynamicState

	DynamicState dynamicState;

	ErlDrvNowData lastUpdate;


private:
	// ----------------------------------------------------------------------------------------------------------------
	// Helpers

	/**
	 * -spec evaluate(TimeDelta, Velocity, Force, Spin, Torque, State) -> {Velocity, Force, Spin, Torque, State} when
	 *     TimeDelta :: float(),
	 *     Velocity :: vector:vec(),
	 *     Force :: vector:vec(),
	 *     Spin :: quaternion:quat(),
	 *     Torque :: vector:vec(),
	 *     State :: #physical{}.
	 */
	void evaluate(DynamicState& dynamicState);


	/**
	 * -spec forces(TimeDelta, State) -> {Force, Torque} when
	 *     TimeDelta :: float(),
	 *     State :: #physical{},
	 *     Force :: vector:vec(),
	 *     Torque :: vector:vec().
	 */

	/**
	 * -spec update_state(State) -> {Spin, AngularVelocity} when
	 *     State :: #physical{},
	 *     Spin :: quaternion:quat(),
	 *     AngularVelocity :: vector:vec().
	 */


	// ----------------------------------------------------------------------------------------------------------------
	// Properties

	// Intrinsic values (should NOT change during the life of an object)
	float mass;
	float inverseMass;
	float inertiaTensor;
	float inverseInertiaTensor;
}; // end Physical


#endif // __PHYSICAL__
