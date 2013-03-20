-include("pre_client.hrl").

-record(entity, {
	id :: binary(),
	client :: #client_info{},
	behavior :: atom(),
	model = [{model, <<"Ships/ares">>}] :: json(),
	state :: [{atom(), json()}]
}).
