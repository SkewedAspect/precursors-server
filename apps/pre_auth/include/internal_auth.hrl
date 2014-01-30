-record(user_auth, {
	username = "" :: string(),
	password = <<>> :: binary(),
	created = erlang:now() :: any(),
	updated = erlang:now() :: any(),
	locked = undefined :: 'undefined' | string(),
	props = [] :: [{any(), any()}]
}).
