-include("pre_client.hrl").

-record(vector, {
	x :: float(),
	y :: float(),
	z :: float()
}).

% A quaternion (w + xi + yj + zk)
-record(quaternion, {
	w :: float(),
	x :: float(),
	y :: float(),
	z :: float()
}).

-record(entity, {
	id :: entity_id(),
	client :: #client_info{},
	position :: #vector{},
	position_vel :: #vector{},
	position_acc_abs :: #vector{},
	position_acc_rel :: #vector{},
	orientation :: #quaternion{},
	orientation_vel :: #quaternion{},
	orientation_acc_abs :: #quaternion{},
	orientation_acc_rel :: #quaternion{}
}).
