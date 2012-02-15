-record(client_connection, {
	pid,
	ssl_socket,
	tcp_socket,
	udp_socket
}).

-record(client_info, {
	connection,
	channel_manager,
	user_id,
	username
}).

-type(message_type() :: 'request' | 'response' | 'event').
-type(message_id() :: any()).
-type(json() :: integer() | float() | binary() | {struct, [{binary(), json()}]} | [json()] | null | true | false).
-record(envelope, {
	type :: message_type(),
	id :: message_id(),
	channel :: binary(),
	contents :: json()
}).
