%%% @doc The client's connection process.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_client).

-behaviour(gen_server).

-include("pre_client.hrl").

% API
-export([start_link/2,
	connect_tcp/3,
	ensure_ets/0,
	get_aes/ 1,
	handle_messages/3,
	handle_message/3,
	send_event/3,
	send_event/4,
	send_response/4,
	send_response/5,
	send_request/4,
	send_request/5
]).

% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-define(SERVER, ?MODULE).

%% ---------------------------------------------------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------------------------------------------------

% We are always started in response to an SSL connection. Our supervisor is also in charge of generating the cookie used
% to identify the incoming TCP connection as this client.
start_link(SslProto, Cookie) ->
	gen_server:start_link(?MODULE, [SslProto, Cookie], []).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Starts the ETS table needed for the TCP cookie lookup.
-spec ensure_ets() -> ok.

ensure_ets() ->
	ets:new(client_ets, [named_table, public]).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Looks up the client pid by Cookie, and informs that pid of the `TcpProto` pid. Then deletes the entry in the ets
%% table. (This helps keep the table nice and small.)
-spec connect_tcp(TcpProto :: pid(), Cookie :: binary(), RequestID :: binary()) -> ClientPid :: pid() | {stop, Reason :: atom()}.

connect_tcp(TcpProto, Cookie, RequestID) ->
	case ets:lookup(client_ets, Cookie) of
		[{_, Pid}] ->
			% Clean ourselves up.
			ets:delete(client_ets, Cookie),

			gen_server:cast(Pid, {tcp_connect, TcpProto, RequestID}),
			Pid;
		_ ->
			lager:error("Failed to look up TCP cookie, disconnecting client."),
			{stop, tcp_cookie_error}
	end.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Retuns the AES key and AES vector in use by the TCP connection.
-spec get_aes(ClientPid :: pid()) -> { AESKey :: binary(), AESVec :: binary() }.

get_aes(ClientPid) ->
	gen_server:call(ClientPid, get_aes).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Processes a list of incoming channel messages.
%% @see handle_message/3
-spec handle_messages(ClientPid :: pid(), Transport :: atom(), Messages :: list()) -> ok.

handle_messages(ClientPid, Transport, Messages) ->
	[handle_message(ClientPid, Transport, Message) || Message <- Messages],
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Process an incoming channel message from the client.
-spec handle_message(ClientPid :: pid(), Trasnport :: atom(), Message :: term()) -> ok.

handle_message(ClientPid, Transport, Message) ->
	gen_server:cast(ClientPid, {Transport, Message}).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Send an event to the client, over tcp.
%% @see send_event/4
-spec send_event(ClientPid :: pid(), Channel :: binary(), Content :: term()) -> ok.

send_event(ClientPid, Channel, Content) ->
	gen_server:cast(ClientPid, { send_event, tcp, Channel, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Send an event to the client. `Content' must be able JSON serializable. (Note: this method is responsible for
%% wrapping `Content' in the envelope structure.)
-spec send_event(ClientPid :: pid(), Transport :: ssl | tcp, Channel :: binary(), Content :: term()) -> ok.

send_event(ClientPid, Transport, Channel, Content) ->
	gen_server:cast(ClientPid, { send_event, Transport, Channel, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Send a response to the client over tcp.
%% @see send_response/5
-spec send_response(ClientPid :: pid(), Channel :: binary(), ID :: any(), Content :: term()) -> ok.

send_response(ClientPid, Channel, ID, Content) ->
	gen_server:cast(ClientPid, { send_response, tcp, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Send a response to the client. `Content' must be able JSON serializable. (Note: this method is responsible for
%% wrapping `Content' in the envelope structure.)
-spec send_response(ClientPid :: pid(), Transport :: ssl | tcp, Channel :: binary(), ID :: any(), Content :: term()) -> ok.

send_response(ClientPid, Transport, Channel, ID, Content) ->
	gen_server:cast(ClientPid, { send_response, Transport, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

%TODO: These functions are here for possible future use, but I'm not sure it EVER makes sense. Maybe remove?

%% @doc Send a request to the client over tcp.
%% @see send_request/5
-spec send_request(ClientPid :: pid(), Channel :: binary(), ID :: any(), Content :: term()) -> ok.

send_request(ClientPid, Channel, ID, Content) ->
	lager:warning("Sending a request to the client. ARE YOU SURE YOU MEANT TO DO THIS?!!!"),
	gen_server:cast(ClientPid, { send_request, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

%TODO: These functions are here for possible future use, but I'm not sure it EVER makes sense. Maybe remove?

%% @doc Send a request to the client. `Content' must be able JSON serializable. (Note: this method is responsible for
%% wrapping `Content' in the envelope structure.)
-spec send_request(ClientPid :: pid(), Transport :: ssl | tcp, Channel :: binary(), ID :: any(), Content :: term()) -> ok.

send_request(ClientPid, Transport, Channel, ID, Content) ->
	lager:warning("Sending a request to the client. ARE YOU SURE YOU MEANT TO DO THIS?!!!"),
	gen_server:cast(ClientPid, { send_request, Transport, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------------------------------------------------

init([SslProto, Cookie]) ->
	% Add ourselves to the ets table, by cookie
	ets:insert(client_ets, {Cookie, self()}),

	% Wait a maximum of 30 seconds for the tcp connection.
	erlang:send_after(30000, self(), check_tcp),

	% Monitor the ssl proto
	erlang:monitor(process, SslProto),

	{ok, #client_state{ssl_proto = SslProto, cookie = Cookie}}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden Inform the client of what our AES key/vec is.
handle_call(get_aes, _From, State) ->
	%TODO: Check `From' and make sure it's our TCPProto?
	{reply, { State#client_state.aes_key, State#client_state.aes_vector }, State};


%% @hidden
handle_call(Request, _From, State) ->
	lager:debug("Unhandled call:  ~p", [Request]),
	{reply, unknown, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_cast({send_event, ssl, Channel, Content}, State) ->
	Envelope = #envelope{type = event, channel = Channel, contents = Content},
	SslProto = State#client_state.ssl_proto,

	% Send a message over SSL
	pre_channels_proto:send_envelope(SslProto, Envelope),

	{noreply, State};


%% @hidden
handle_cast({send_event, tcp, Channel, Content}, State) ->
	Envelope = #envelope{type = event, channel = Channel, contents = Content},
	TcpProto = State#client_state.tcp_proto,

	% Send a message over TCP
	pre_channels_proto:send_envelope(TcpProto, Envelope),

	{noreply, State};

%% @hidden
handle_cast({send_response, ssl, Channel, ID, Content}, State) ->
	Envelope = #envelope{type = response, channel = Channel, id = ID, contents = Content},
	SslProto = State#client_state.ssl_proto,

	% Send a message over SSL
	pre_channels_proto:send_envelope(SslProto, Envelope),

	{noreply, State};


%% @hidden
handle_cast({send_response, tcp, Channel, ID, Content}, State) ->
	Envelope = #envelope{type = response, channel = Channel, id = ID, contents = Content},
	TcpProto = State#client_state.tcp_proto,

	% Send a message over TCP
	pre_channels_proto:send_envelope(TcpProto, Envelope),

	{noreply, State};


%% @hidden
handle_cast({tcp_connect, TcpProto, RequestID}, State) ->

	% Send response to the client
	send_response(self(), ssl, <<"control">>, RequestID, [{confirm, true}]),

	% Set the TCP Proto
	State1 = State#client_state{tcp_proto = TcpProto},
	{noreply, State1};


%% @hidden
handle_cast({ssl, Message}, State) ->
	State1 = process_channel(Message, State),
	{noreply, State1};


%% @hidden
handle_cast({tcp, #envelope{channel = <<"control">>} = Message}, State) ->
	lager:error("Control message received over TCP! Someone's doing something bad! Message: ~p", [Message]),

	% Drop the client! It's either trying to do something bad, or there's a bug in it's code. Either way, die in a fire.
	{stop, control_msg_over_tcp, State};


%% @hidden
handle_cast({tcp, Message}, State) ->
	State1 = process_channel(Message, State),
	{noreply, State1};


%% @hidden
handle_cast(Request, State) ->
	lager:debug("Unhandled cast:  ~p", [Request]),
	{noreply, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_info(check_tcp, State) ->
	case State#client_state.tcp_proto of
		undefined ->
			% Inform the client of the error.
			send_event(self(), ssl, <<"control">>, [
				{type, <<"error">>},
				{message, <<"TCP failed to connect in a timely manner.">>}
			]),

			% Gracefully exit
			{stop, normal, State};
		_ ->
			{noreply, State}
	end;


%% @hidden If the ssl connection closes, the client connection needs to close as well.
handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
	SslProto = State#client_state.ssl_proto,
	lager:warning("SslProto ~p exited. Gracefully shutting down client connection process.", [SslProto]),
	{stop, normal, State};


%% @hidden
handle_info(Info, State) ->
	lager:warning("Unhandled message: ~p", [Info]),
	{noreply, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden
terminate(Reason, State) ->
	% In the event that we are exiting before TCP has connected, clean up after ourselves.
	case State#client_state.tcp_proto of
		undefined ->
			ets:delete(client_ets, State#client_state.cookie);
		_ ->
			ok
	end,

	% Log the exit
	case Reason of
		normal ->
			lager:debug("Client disconnected.");
		_ ->
			lager:warning("Client disconnected unexpectedly! Reason: ~p", [Reason])
	end,

	% Cleanup our entity
	Entity = State#client_state.entity,
	case Entity of
		undefined ->
			ok;
		_ ->
			pre_entity_balancer:notify(remove, Entity:id(), client_exit),
			ok
	end.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden Send the message off to a callback module.
process_channel(#envelope{channel = <<"control">>} = Message, State) ->
	process_channel(pre_control_channel, Message, State);

%% @hidden
process_channel(#envelope{channel = <<"entity">>} = Message, State) ->
	process_channel(pre_entity_channel, Message, State);

%% @hidden
process_channel(#envelope{channel = <<"input">>} = Message, State) ->
	process_channel(pre_input_channel, Message, State);

%% @hidden
process_channel(#envelope{channel = <<"ping">>} = Message, State) ->
	process_channel(pre_ping_channel, Message, State);

%% @hidden
process_channel(#envelope{channel = <<"chat">>} = Message, State) ->
	process_channel(pre_ping_chat, Message, State);

%% @hidden
process_channel(Message, State) ->
	lager:warning("Got message for unknown channel: ~p", [Message]),
	State.


%% @hidden
process_channel(ChannelModule, Message, State) ->
	Contents = Message#envelope.contents,
	ReqType = proplists:get_value(type, Contents),

	case Message#envelope.type of
		 event ->
			 ChannelModule:handle_event(ReqType, Contents, State);
		 request ->
			 ChannelModule:handle_request(ReqType, Message#envelope.id, Contents, State);
		 response ->
			 ChannelModule:handle_response(ReqType, Message#envelope.id, Contents, State);
		 _ ->
			 lager:warning("Unknown Message Type: ~p", [Message#envelope.type])
	end.


