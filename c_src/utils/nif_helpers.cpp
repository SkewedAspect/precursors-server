/**
 * @doc NIF helpers
 *
 * @copyright 2013 David H. Bronke
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#include "nif_helpers.h"


bool getNIFDouble(ErlNifEnv* env, const ERL_NIF_TERM term, double* target)
{
	unsigned long ulongOther;
	long longOther;
	ErlNifUInt64 uint64Other;
	ErlNifSInt64 int64Other;

	if(!enif_get_double(env, term, target))
	{
		// Couldn't get a double directly; try the integer types...
		if(enif_get_ulong(env, term, &ulongOther))
		{
			*target = ulongOther;
		}
		else if(enif_get_long(env, term, &longOther))
		{
			*target = longOther;
		}
		else if(enif_get_uint64(env, term, &uint64Other))
		{
			*target = uint64Other;
		}
		else if(enif_get_int64(env, term, &int64Other))
		{
			*target = int64Other;
		}
		else
		{
			// Nothing matched what we were expecting; fail.
			return false;
		} // end if
	} // end if

	return true;
} // end getNIFDouble
