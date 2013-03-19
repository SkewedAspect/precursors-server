-include("pre_client.hrl").

-record(entity, {
	id :: binary(),
	client :: #client_info{},
	behavior :: atom(),
	definition :: {Bucket :: binary(), Key :: binary()},
	state :: json(),
	watchers :: [pid()]
}).
