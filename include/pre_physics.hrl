-record(physical, {
	position :: vector:vec(),
	position_vel :: vector:vec(),
	position_acc_abs :: vector:vec(),
	position_acc_rel :: vector:vec(),
	orientation :: quaternion:quat(),
	orientation_vel :: quaternion:quat(),
	orientation_acc_abs :: quaternion:quat(),
	orientation_acc_rel :: quaternion:quat()
}).
