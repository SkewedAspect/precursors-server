/**
 * A quaternion. (w + xi + yj + zk)
 */
class Quat
{
public:
	Quat();

	Quat(double w, double x, double y, double z);

	Quat(ErlNifEnv* env, const ERL_NIF_TERM);

	// Conversion
	bool fromTerm(ErlNifEnv* env, const ERL_NIF_TERM term);

	ERL_NIF_TERM toTerm(ErlNifEnv* env);

	// Operations with other `Quat`s
	Quat add(const Quat& other) const;
	Quat subtract(const Quat& other) const;
	Quat multiply(const Quat& other) const;

	// Operations with Erlang terms
	Quat multiply(ErlNifEnv* env, const ERL_NIF_TERM other) const;

	// Operations with other types
	Quat multiply(const double other) const;
	Quat multiply(const int32_t other) const;
	Quat multiply(const u_int32_t other) const;
	Quat multiply(const int64_t other) const;
	Quat multiply(const u_int64_t other) const;

	// Operator overloads (Quat <op> Quat)
	Quat operator+(const Quat& rhs) const { return add(rhs); }
	Quat operator-(const Quat& rhs) const { return subtract(rhs); }
	Quat operator*(const Quat& rhs) const { return multiply(rhs); }

	// Operator overloads (Quat <op> <other>)
	Quat operator*(const double rhs) const { return multiply(rhs); }

	// Components
	double w;
	double x;
	double y;
	double z;
}; // end Quat

static Quat operator*(const double lhs, const Quat& rhs);
