-include("pre_client.hrl").

-record(entity, {
	id :: binary(),
	client :: #client_info{},
	behavior :: atom(),
	definition :: {Bucket :: binary(), Key :: binary()},
	model = [{model, <<"Ships/ares">>}] :: json(),
	state :: json(),
	watchers :: [pid()]
}).
