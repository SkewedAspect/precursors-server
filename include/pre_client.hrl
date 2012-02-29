-record(client_connection, {
	pid,
	ssl_socket,
	tcp_socket,
	udp_socket
}).

-record(character_info, {
	id :: any(),
	name :: binary()
}).

-record(client_info, {
	connection :: pid(),
	channel_manager :: pid(),
	user_id :: any(),
	username :: binary(),
	show_account :: 'hidden' | 'public',
	character :: #character_info{}
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
