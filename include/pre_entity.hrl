-include("pre_client.hrl").

-record(entity, {
	id :: binary(),
	client :: #client_info{},
	behavior :: atom(),
	state :: dict:new() | dict()
}).
