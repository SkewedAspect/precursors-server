-include_lib("pre_communication_layer/include/pre_client.hrl").

-record(entity, {
	id :: binary(),
	client :: pid(),
	controller :: atom(),
	state :: any(),
	latest_update = [] :: list()
}).
