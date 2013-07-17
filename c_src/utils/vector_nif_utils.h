#ifndef __VECTOR_NIF_UTILS__
#define __VECTOR_NIF_UTILS__

#include "Vec.h"


// Conversion
bool termToVec(ErlNifEnv* env, const ERL_NIF_TERM term, Vec& targetVec);

inline ERL_NIF_TERM vecToTerm(ErlNifEnv* env, const Vec& vec)
{
	return enif_make_tuple3(env,
			enif_make_double(env, vec.x),
			enif_make_double(env, vec.y),
			enif_make_double(env, vec.z)
			);
} // end vecToTerm


#endif // __VECTOR_NIF_UTILS__
