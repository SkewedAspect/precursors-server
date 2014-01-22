-include("pre_client.hrl").

-record(entity, {
	id :: binary(),
	client :: #client_info{},
	controller :: atom(),
	state :: any(),
	latest_update = [] :: list()
}).
