-include("pre_client.hrl").

-record(vector, {
	x :: float(),
	y :: float(),
	z :: float()
}).

% A quaternion (a + bi + cj + dk)
-record(quaternion, {
	a :: float(),
	b :: float(),
	c :: float(),
	d :: float()
}).

-record(entity, {
	id :: {pid(), term()},
	client :: #client_info{},
	pos :: #vector{},
	pos_vel :: #vector{},
	pos_acc :: #vector{},
	orient :: #quaternion{},
	orient_vel :: #quaternion{},
	orient_acc :: #quaternion{}
}).
