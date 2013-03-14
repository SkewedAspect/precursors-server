-include("pre_client.hrl").
-include("pre_physics.hrl").

-record(entity, {
	id :: #entity_id{},
	client :: #client_info{},
	physical = #physical{} :: #physical{},
	model_def = [{model, <<"Ships/ares">>}] :: json_object(),
	callback_module = entity_test :: module(),
	behavior_data :: any()
}).
