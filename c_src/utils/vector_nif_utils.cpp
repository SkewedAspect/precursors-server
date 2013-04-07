#include "nif_helpers.h"

#include "vector_nif_utils.h"


// --------------------------------------------------------------------------------------------------------------------
// Helpers

bool termToVec(ErlNifEnv* env, const ERL_NIF_TERM term, Vec& targetVec)
{
	int arity;
	const ERL_NIF_TERM* array;

	if(!enif_get_tuple(env, term, &arity, &array))
	{
		return false;
	} // end if

	if(arity != 3)
	{
		return false;
	} // end if

	if(!getNIFDouble(env, array[0], &targetVec.x)
			|| !getNIFDouble(env, array[1], &targetVec.y)
			|| !getNIFDouble(env, array[2], &targetVec.z))
	{
		return false;
	} // end if

	return true;
} // termToVec
