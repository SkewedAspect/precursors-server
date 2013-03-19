-record(client_info, {
	connection :: pid(),
	channel_manager :: pid(),
	user_id :: any(),
	username :: binary(),
	show_account :: 'hidden' | 'public',
	character_id :: binary(),
	character :: json(),
	entity :: binary(),
	entity_engine :: pid()
}).

%%-----------------------------------------------------------------------
%% These records should ONLY be used by pre_client_connection and
%% pre_tcp_transient!

-record(client_connection, {
	pid,
	ssl_socket,
	tcp_socket,
	udp_socket
}).

-type(message_type() :: 'request' | 'response' | 'event').
-type(message_id() :: any()).
-record(envelope, {
	type :: message_type(),
	id :: message_id(),
	channel :: binary(),
	contents :: json()
}).

%%-----------------------------------------------------------------------
