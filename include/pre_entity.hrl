-include("pre_client.hrl").
-include("pre_physics.hrl").

-record(entity, {
	id :: entity_id(),
	client :: #client_info{},
	physical :: #physical{}
}).
