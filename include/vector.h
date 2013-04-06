/* A vector (x, y, z) */
typedef struct
{
	double x;
	double y;
	double z;
} Vec;


static Vec* termToVec(ErlNifEnv* env, ERL_NIF_TERM term, Vec* targetVec);

static ERL_NIF_TERM vecToTerm(ErlNifEnv* env, Vec* vec);
