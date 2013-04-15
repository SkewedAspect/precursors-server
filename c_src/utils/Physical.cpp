/**
 * @doc The Physical class
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include "Physical.h"


// Constructor
Physical::Physical() :
	forceAbsolute(Vec()),
	forceRelative(Vec()),
	torqueAbsolute(Vec()),
	torqueRelative(Vec()),
	dynamicState(DynamicState()),
	mass(1),
	inverseMass(1),
	inertiaTensor(1),
	inverseInertiaTensor(1)
{
	if(!driver_get_now(&lastUpdate))
	{
		//FIXME: If this happens, run for the hills. Programming is broken.
	} // end if
} // end Physical


// --------------------------------------------------------------------------------------------------------------------
// In-place modifications

void Physical::setMass(float mass)
{
	this->mass = mass;
	inverseMass = 1 / mass;
} // end setMass

void Physical::setInertiaTensor(float inertiaTensor)
{
	this->inertiaTensor = inertiaTensor;
	inverseInertiaTensor = 1 / inertiaTensor;
} // end setInertiaTensor

/**
 * Simulate physical movement of the 'physical' object represented by `InitialPhysical`, over the time since that
 * object's 'lastUpdate' timestamp, or over the given `timeDelta` (in microseconds).
 */
void Physical::simulate(unsigned long timeDelta_us)
{
	if(timeDelta_us == (unsigned long) -1)
	{
		timeDelta_us = timeSinceLastUpdate();
	} // end if

} // end simulate


// --------------------------------------------------------------------------------------------------------------------
// Operations that produce other values

unsigned long Physical::timeSinceLastUpdate() const
{
	ErlDrvNowData now;
	if(!driver_get_now(&now))
	{
		//FIXME: If this happens, run for the hills. Programming is broken.
		return 0;
	} // end if

	return (
			(now.megasecs - lastUpdate.megasecs) * 1000000
			+ now.secs - lastUpdate.secs
		) * 1000000
		+ now.microsecs - lastUpdate.microsecs;
} // end timeSinceLastUpdate


// ----------------------------------------------------------------------------------------------------------------
// Helpers

void Physical::evaluate(Physical::DynamicState& dynamicState)
{
} // end evaluate


// --------------------------------------------------------------------------------------------------------------------
// Properties

Physical::DynamicState::DynamicState() :
	position(Vec()),
	linearMomentum(Vec()),
	orientation(Quat()),
	angularMomentum(Vec()),
	linearVelocity(Vec()),
	angularVelocity(Vec()),
	spin(Quat())
{
} // end DynamicState
