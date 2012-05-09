-record(physical, {
	% Updated values (assume these change every frame)
	position = {0, 0, 0} :: vector:vec(),
	linear_momentum = {0, 0, 0} :: vector:vec(),
	orientation = {1, 0, 0, 0} :: quaternion:quat(),
	angular_momentum = {0, 0, 0} :: vector:vec(),

	% Input-only values
	force_absolute = {0, 0, 0} :: vector:vec(),
	force_relative = {0, 0, 0} :: vector:vec(),
	torque_absolute = {0, 0, 0} :: vector:vec(),
	torque_relative = {0, 0, 0} :: vector:vec(),

	% Purely calculated values (DON'T try to change these externally)
	last_update :: erlang:timestamp(),
	linear_velocity = {0, 0, 0} :: vector:vec(),
	angular_velocity = {0, 0, 0} :: vector:vec(),
	spin = {1, 0, 0, 0} :: quaternion:quat(),

	% Intrinsic values (should NOT change during the life of an object)
	mass = 1 :: float(),
	inverse_mass = 1 :: float(),
	inertia_tensor = 1 :: float(),
	inverse_inertia_tensor = 1 :: float()
}).
