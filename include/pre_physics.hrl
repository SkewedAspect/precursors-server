-record(physical, {
	position = {0, 0, 0} :: vector:vec(),
	position_vel = {0, 0, 0} :: vector:vec(),
	position_acc_abs = {0, 0, 0} :: vector:vec(),
	position_acc_rel = {0, 0, 0} :: vector:vec(),
	orientation = {0, 0, 0, 0} :: quaternion:quat(),
	orientation_vel = {0, 0, 0, 0} :: quaternion:quat(),
	orientation_acc_abs = {0, 0, 0, 0} :: quaternion:quat(),
	orientation_acc_rel = {0, 0, 0, 0} :: quaternion:quat()
}).
