#ifndef __NIF_HELPERS__
#define __NIF_HELPERS__

#include <erl_nif.h>


/* ----------------------------------------------------------------------------------------------------------------- */
/* Metaprogramming is my second best friend. */

/**
 * Fail with a 'badarg' exception.
 */
#define FAIL return enif_make_badarg(env)

/**
 * Fail with a 'badarg' exception if the given expression is false.
 */
#define FAIL_IF(EXPR) if(EXPR) { FAIL; }

/**
 * If SAFER_THAN_NECESSARY is defined, CHECK_ARGC(N) ensures that the correct number of arguments are present in an
 * Erlang interface function.
 */
#ifdef SAFER_THAN_NECESSARY
#	define CHECK_ARGC(N) FAIL_IF(argc != N)
#else
#	define CHECK_ARGC(N)
#endif


/// Helper for getting a double out of an ERL_NIF_TERM, even if the term is actually storing some sort of int.
bool getNIFDouble(ErlNifEnv* env, const ERL_NIF_TERM term, double* target);

#endif // __NIF_HELPERS__
