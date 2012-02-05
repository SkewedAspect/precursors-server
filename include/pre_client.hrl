-record(client_connection, {
	pid,
	ssl_socket,
	tcp_socket,
	udp_socket
}).

-type(message_type() :: 'request' | 'response' | 'event').
-type(message_id() :: 'undefined' | integer()).
-type(json() :: integer() | float() | binary() | {struct, [{binary(), json()}]} | [json()] | null | true | false).
-record(envelope, {
	type :: message_type(),
	id :: message_id(),
	channel :: binary(),
	contents :: json()
}).
