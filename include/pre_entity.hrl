-include("pre_client.hrl").
-include("pre_physics.hrl").

-record(entity, {
	id :: #entity_id{},
	client :: #client_info{},
	physical = #physical{} :: #physical{},
	model_def = [{model, <<"Ships/ares">>}] :: [{atom(), term()}],
	callback_module = entity_test :: module()
}).
