%%% @doc This header is for working with data internal to the `pre_client' process, and the channels callbacks.
%%% --------------------------------------------------------------------------------------------------------------------

-type(message_type() :: 'request' | 'response' | 'event').
-type(message_id() :: any()).
-record(envelope, {
	type :: message_type(),
	id :: message_id(),
	channel :: binary(),
	contents %:: json()
}).

%% ---------------------------------------------------------------------------------------------------------------------

-record(client_state, {
	account :: term(),
	cookie :: binary(),
	ssl_proto :: pid(),
	tcp_proto :: pid(),
	aes_key :: binary(),
	aes_vector :: binary()
}).

