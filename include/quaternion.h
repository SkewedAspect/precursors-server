/* A quaternion (w + xi + yj + zk) */
typedef struct
{
	double w;
	double x;
	double y;
	double z;
} Quat;


static Quat* termToQuat(ErlNifEnv* env, ERL_NIF_TERM term, Quat* targetQuat);

static ERL_NIF_TERM quatToTerm(ErlNifEnv* env, Quat quat);

static Quat add(const Quat q1, const Quat q2);
