-module(pre_client_connection).
-behaviour(gen_server).

-include("log.hrl").
-include("pre_client.hrl").

-define(AES_BLOCK_SIZE, 16).

-record(state, {
	cookie :: binary(),
	ssl_socket,
	ssl_netstring,
	tcp_socket,
	tcp_netstring,
	udp_socket,
	udp_remote_info :: binary() | {string(), integer()} | 'undefined',
	aes_key :: binary(),
	aes_vector :: binary(),
	channel_mgr :: pid(),
	client_info = #client_info{} :: #client_info{}
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, start/2, set_tcp/5, send/5, set_inhabited_entity/2, json_to_envelope/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts a new client connection using the given socket and cookie.

-spec start_link(Socket, Cookie) -> {'ok', pid()} when
	Socket :: any(),
	Cookie :: binary().

start_link(Socket, Cookie) ->
	gen_server:start_link(?MODULE, {Socket, Cookie}, []).

%% ------------------------------------------------------------------

%% @doc Same as {@link start_link/2} only is not linked to the calling
%% process.

-spec start(Socket, Cookie) -> {'ok', pid()} when
	Socket :: any(),
	Cookie :: binary().

start(Socket, Cookie) ->
	gen_server:start(?MODULE, {Socket, Cookie}, {}).

%% ------------------------------------------------------------------

%% @doc A transient tcp connection will use this to inform the client
%% connection that a client has successfully connected and authed with
%% a cookie.

-spec set_tcp(Pid, Socket, Message, Bins, Cont) -> 'ok' when
	Pid :: pid(),
	Socket :: any(),
	Message :: json(),
	Bins :: [binary()],
	Cont :: any().

set_tcp(Pid, Socket, Message, Bins, Cont) ->
	gen_server:cast(Pid, {set_tcp, Socket, Message, Bins, Cont}).

%% ------------------------------------------------------------------

%% @doc Send a message out to the client over a given channel.  If the
%% type is 'request', an id is automatically generated.

-spec send(Pid, Socket, Type, Channel, Json) -> 'ok' | message_id() when
	Pid :: pid() | #client_info{},
	Socket :: 'udp' | 'ssl' | 'tcp',
	Type :: 'request' | {'response', message_id()} | 'event' | {'request', message_id()},
	Channel :: binary(),
	Json :: json().

send(ClientInfo, S, R, C, J) when is_record(ClientInfo, client_info) ->
	Pid = ClientInfo#client_info.connection,
	send(Pid, S, R, C, J);

send(Pid, Socket, request, Channel, Json) when Socket == udp; Socket == ssl; Socket == tcp ->
	Id = generate_id(),
	send(Pid, Socket, {request, Id}, Channel, Json),
	Id;

send(Pid, Socket, Type, Channel, Json) when Socket == udp; Socket == ssl; Socket == tcp ->
	gen_server:cast(Pid, {send, Socket, Type, Channel, Json}).

%% ------------------------------------------------------------------

%% @doc Set the given client's inhabited entity.

-spec set_inhabited_entity(Pid, EntityID) -> 'ok' when
	Pid :: pid() | #client_info{},
	EntityID :: #entity_id{}.

set_inhabited_entity(Pid, EntityID) ->
	gen_server:cast(Pid, {inhabit_entity, EntityID}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @hidden
init({Socket, Cookie}) ->
	?info("New client connection"),
	ssl:setopts(Socket, [{active, once}]),
	{ok, Udp} = gen_udp:open(0, [{active, once}, binary, {ip, {0,0,0,0}}]),
	ClientInfo = #client_info{
		connection = self()
	},
	State = #state{
		ssl_socket = Socket,
		cookie = Cookie,
		udp_socket = Udp,
		client_info = ClientInfo
	},
	pre_hooks:async_trigger_hooks(client_connected, [ClientInfo], all),
	{ok, State, 1000}.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @hidden
handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

%% @hidden
handle_cast({send, tcp, Type, Channel, Json}, #state{tcp_socket = undefined} = State) ->
	?warning("Tried to send ~p message for channel ~p over TCP before getting TCP sync info: ~p", [Type, Channel, Json]),
	{noreply, State};

handle_cast({send, tcp, Type, Channel, Json}, State) ->
	#state{aes_key = AESKey, aes_vector = AESVector} = State,
	Bin = build_message(Type, Channel, Json, AESKey, AESVector),
	gen_tcp:send(State#state.tcp_socket, Bin),
	{noreply, State};

handle_cast({send, ssl, Type, Channel, Json}, State) ->
	Bin = build_message(Type, Channel, Json),
	ssl:send(State#state.ssl_socket, Bin),
	{noreply, State};

handle_cast({send, udp, Type, Channel, Json}, #state{udp_remote_info = undefined} = State) ->
	?warning("Tried to send ~p message for channel ~p over UDP before getting UDP sync info: ~p", [Type, Channel, Json]),
	{noreply, State};

handle_cast({send, udp, Type, Channel, Json}, State) ->
	#state{udp_socket = Socket, udp_remote_info = {Ip, Port}, aes_key = AESKey, aes_vector = AESVector} = State,
	Bin = build_message(Type, Channel, Json, AESKey, AESVector, no_netstring),
	gen_udp:send(Socket, Ip, Port, Bin),
	{noreply, State};

handle_cast({set_tcp, Socket, Message, Bins, Cont}, State) ->
	% Respond to connect message
	confirm_connect_message(Message, State),
	% Update state and handle any remaining requests
	State1 = State#state{tcp_socket = Socket, tcp_netstring = Cont},
	NewState = service_messages(Bins, State1),
	{noreply, NewState};

handle_cast(start_accept_tcp, State) ->
	#state{tcp_socket = Socket} = State,
	inet:setopts(Socket, [{active, once}]),
	ClientRec = #client_connection{
		pid = self(),
		ssl_socket = State#state.ssl_socket,
		tcp_socket = Socket,
		udp_socket = case State#state.udp_remote_info of
			undefined -> State#state.cookie;
			Else -> Else
		end
	},
	ets:insert(client_ets, ClientRec),
	?info("TCP socket set:  ~p", [Socket]),
	{noreply, State};

handle_cast({inhabit_entity, EntityID}, State) ->
	ClientInfo = State#state.client_info,
	pre_hooks:async_trigger_hooks(client_inhabited_entity, [self(), EntityID], all),
	NewState = State#state{
		client_info = ClientInfo#client_info{entity = EntityID}
	},
	{noreply, NewState};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

%% @hidden
handle_info({ssl, Socket, Packet}, #state{ssl_socket = Socket, ssl_netstring = Cont} = State) ->
	{Bins, SSLNetstring} = parse_netstring(Packet, Cont),
	State1 = State#state{ssl_netstring = SSLNetstring},
	NewState = service_messages(Bins, State1),
	ssl:setopts(Socket, [{active, once}, binary]),
	{noreply, NewState};

handle_info({ssl_closed, Socket}, #state{ssl_socket = Socket} = State) ->
	?info("Got SSL socket closed; Imma die"),
	{stop, normal, State};

handle_info({tcp, Socket, Packet}, #state{tcp_socket = Socket, tcp_netstring = Cont} = State) ->
	{Bins, TCPNetstring} = parse_netstring(Packet, Cont),
	State1 = State#state{tcp_netstring = TCPNetstring},
	Messages = [aes_decrypt_envelope(Bin, State) || Bin <- Bins],
	NewState = service_messages(Messages, State1),
	inet:setopts(Socket, [{active, once}]),
	{noreply, NewState};

handle_info({udp, Socket, Ip, InPortNo, Packet},
	#state{udp_socket = Socket, udp_remote_info = undefined} = State) ->
		% Decrypt message envelope
		Message = aes_decrypt_envelope(Packet, State),
		Success = handle_connect_message(Message, State),
		State1 = case Success of
			{ok, State0} ->
				% Record this client's information in ETS
				ClientRec = #client_connection{
					pid = self(),
					ssl_socket = State#state.ssl_socket,
					tcp_socket = case State#state.tcp_socket of
						undefined -> State#state.cookie;
						TcpElse -> TcpElse
					end,
					udp_socket = {Ip, InPortNo}
				},
				ets:insert(client_ets, ClientRec),
				?info("UDP port sync up:  ~p:~p", [Ip, InPortNo]),
				% Record the UDP information in the state
				State0#state{udp_remote_info = {Ip, InPortNo}};
			Else ->
				?info("UDP not confirmed:  ~p", [Else]),
				State
		end,
		inet:setopts(Socket, [{active, once}]),
		{noreply, State1};

handle_info({udp, Socket, Ip, InPortNo, Packet}, State) ->
	#state{
		udp_socket = Socket,
		udp_remote_info = {Ip, InPortNo}
	} = State,
	Message = aes_decrypt_envelope(Packet, State),
	?warning("Client got unhandled UDP message:  ~p", [Message]),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State};

handle_info(timeout, State) ->
	?warning("Client did not respond in time; disconnecting."),
	{stop, timeout, State};

handle_info(Info, State) ->
	?debug("Unhandled info:  ~p (~p)", [Info, State]),
	{noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

%% @hidden
terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

service_messages([], State) ->
	State;
service_messages([Binary | Tail], State) ->
	NewState = service_message(Binary, State),
	service_messages(Tail, NewState).

%% ------------------------------------------------------------------

service_message(Binary, State) when is_binary(Binary) ->
	Rec = json_to_envelope(Binary),
	service_message(Rec, State);

service_message(#envelope{channel = <<"control">>} = Envelope, State) ->
	service_control_channel(Envelope, State);

service_message(Envelope, State) when is_record(Envelope, envelope) ->
	#state{client_info = ClientInfo, channel_mgr = ChannelMgr} = State,
	#envelope{type = MessageType, channel = Channel, contents = Contents, id = Id} = Envelope,
	case MessageType of
		request ->
			pre_client_channels:handle_request(ChannelMgr, ClientInfo, Channel, Id, Contents);
		response ->
			pre_client_channels:handle_response(ChannelMgr, ClientInfo, Channel, Id, Contents);
		event ->
			pre_client_channels:handle_event(ChannelMgr, ClientInfo, Channel, Contents)
	end,
	State;

service_message(Request, State) ->
	?warning("Unhandled request with invalid envelope:  ~p", [Request]),
	State.

%% ------------------------------------------------------------------

service_control_channel(#envelope{type = Type, id = Id, contents = {struct, Request}}, State) ->
	service_control_message(Type, Id, Request, State);

service_control_channel(Thing, State) ->
	?warning("Unhandled input:  ~p", [Thing]),
	State.

%% ------------------------------------------------------------------

service_control_message(Type, Id, Request, State) when Type =:= request; Type =:= event ->
	ReqType = proplists:get_value(<<"type">>, Request),
	service_control_message(Type, ReqType, Id, Request, State);

service_control_message(Type, _, Request, State) ->
	?warning("Unhandled ~p message:  ~p", [Type, Request]),
	State.

%% ------------------------------------------------------------------

service_control_message(request, <<"login">>, Id, Request, State) ->
	?info("starting authentication"),
	?info("Request: ~p", [Request]),
	% Check with authentication backends
	Username = proplists:get_value(<<"user">>, Request),
	Password= proplists:get_value(<<"password">>, Request),
	?info("Authenticating user: ~p", [Username]),
	{Confirm, Reason} = case pre_gen_auth:authenticate(Username, Password) of
		allow ->
			{true, undefined};
		{deny, Msg} ->
			{false, list_to_binary(Msg)};
		_ ->
			?warning("Authentication failed for unkown reason."),
			{false, <<"An unkown error has occured.">>}
	end,

	% Send login response
	#state{cookie = Cookie, udp_socket = UdpSocket, client_info = ClientInfo} = State,
	{ok, UdpPort} = inet:port(UdpSocket),
	LoginRep = {struct, [
		{confirm, Confirm},
		{reason, Reason},
		{cookie, Cookie},
		{udpPort, UdpPort},
		{tcpPort, 6007}
	]},
	Response = #envelope{id = Id, type = response, contents = LoginRep,
		channel = <<"control">>},
	OutBin = wrap_for_send(Response),
	SendRes = ssl:send(State#state.ssl_socket, OutBin),
	?debug("Login response ssl:send result:  ~p", [SendRes]),

	% Record login information in state
	AESKey = base64:decode(proplists:get_value(<<"key">>, Request)),
	AESVector = base64:decode(proplists:get_value(<<"vector">>, Request)),
	State#state{
		cookie = Cookie,
		udp_remote_info = undefined,
		aes_key = AESKey,
		aes_vector = AESVector,
		client_info = ClientInfo#client_info{
			username = Username
		}
	};

service_control_message(event, <<"logout">>, _, _, _) ->
	?info("Got logout event from client."),
	exit(normal).

%% ------------------------------------------------------------------

generate_id() ->
	IdRef = erlang:make_ref(),
	IdList = io_lib:format("~p", [IdRef]),
	Id = lists:flatten(IdList),
	list_to_binary(Id).

build_message(Type, Channel, Json) ->
	build_message(Type, Channel, Json, undefined, undefined).

build_message(Type, Channel, Json, AESKey, AESVector) ->
	build_message(Type, Channel, Json, AESKey, AESVector, true).

build_message({Type, Id}, Channel, Json, AESKey, AESVector, UseNetstring) ->
	Response = #envelope{id = Id, type = Type, contents = Json, channel = Channel},
	wrap_for_send(Response, AESKey, AESVector, UseNetstring);

build_message(Type, Channel, Json, AESKey, AESVector, UseNetstring) ->
	build_message({Type, generate_id()}, Channel, Json, AESKey, AESVector, UseNetstring).

wrap_for_send(Recthing) ->
	wrap_for_send(Recthing, undefined, undefined, true).

wrap_for_send(Recthing, AESKey, AESVector, UseNetstring) ->
	Json = envelope_to_json(Recthing),
	JsonEnc = mochijson2:encode(Json),
	Binary = list_to_binary(lists:flatten(JsonEnc)),
	EncryptedIfNeeded = case {AESKey, AESVector} of
		{undefined, undefined} ->
			Binary;
		{_, _} ->
			aes_encrypt(Binary, AESKey, AESVector)
	end,
	case UseNetstring of
		true -> netstring:encode(EncryptedIfNeeded);
		_ -> EncryptedIfNeeded
	end.

%% ------------------------------------------------------------------

parse_netstring(Packet, undefined) ->
	parse_netstring(Packet, 10);
parse_netstring(Packet, Continuation) ->
	netstring:decode(Packet, Continuation).

envelope_to_json(Envelope) ->
	#envelope{type = Type, channel = Channel, contents = Contents, id = Id} = Envelope,
	Props = [{<<"type">>, Type},
		{<<"channel">>, Channel},
		{<<"contents">>, Contents}
	],
	case Id of
		undefined -> {struct, Props};
		Id -> {struct, [{<<"id">>, Id} | Props]}
	end.

%% @doc Takes a json and turns it into an envelope record.
-spec(json_to_envelope/1 :: (Json :: binary()) -> #envelope{}).
json_to_envelope(Json) when is_binary(Json) ->
	json_to_envelope(mochijson2:decode(Json));

json_to_envelope({struct, Props}) ->
	Type = proplists:get_value(<<"type">>, Props, <<>>),
	Type0 = check_envelope_type(Type),
	Id = proplists:get_value(<<"id">>, Props),
	Contents = proplists:get_value(<<"contents">>, Props),
	Channel = proplists:get_value(<<"channel">>, Props),
	#envelope{type = Type0, id = Id, contents = Contents, channel = Channel}.

check_envelope_type(<<"request">>) -> request;
check_envelope_type(<<"response">>) -> response;
check_envelope_type(<<"event">>) -> event.

%% ------------------------------------------------------------------

get_pkcs5_padding(Packet) ->
	PacketLength = byte_size(Packet),
	PaddingNeeded = ?AES_BLOCK_SIZE - (PacketLength rem ?AES_BLOCK_SIZE),
	binary:copy(<<PaddingNeeded/integer>>, PaddingNeeded).

% Decrypt message
aes_encrypt(Packet, AESKey, AESVector) ->
	% Decrypt message
	Padding = get_pkcs5_padding(Packet),
	Padded = <<Packet/binary, Padding/binary>>,
	crypto:aes_cbc_128_encrypt(AESKey, AESVector, Padded).

% Decrypt message
aes_decrypt(Packet, #state{aes_key = AESKey, aes_vector = AESVector}) ->
	% Decrypt message
	Decrypted = crypto:aes_cbc_128_decrypt(AESKey, AESVector, Packet),
	% Strip padding added by OpenSSL's AES algorithm.
	PaddingByteCount = binary:last(Decrypted),
	binary:part(Decrypted, 0, byte_size(Decrypted) - PaddingByteCount).

% Decode JSON message
aes_decrypt_envelope(Packet, State) ->
	json_to_envelope(aes_decrypt(Packet, State)).

%% ------------------------------------------------------------------

handle_connect_message(Message, State) ->
	#state{cookie = Cookie} = State,
	try begin
		#envelope{type = request, channel = <<"control">>, contents = {struct, Request}} = Message,
		case proplists:get_value(<<"type">>, Request) of
			<<"connect">> ->
				case proplists:get_value(<<"cookie">>, Request) of
					Cookie -> ok;
					_ -> badcookie
				end;
			_ -> badrequest
		end end,
		confirm_connect_message(Message, State)
	catch
		What:Why ->
			{What,Why}
	end.

confirm_connect_message(#envelope{type = request, channel = <<"control">>} = Message, State) ->
	#state{ssl_socket = SSLSocket, tcp_socket = TCPSocket, udp_socket = UDPSocket, client_info = ClientInfo} = State,
	% Send response over SSL transport
	ConnectRep = {struct, [
		{confirm, true}
	]},
	OutBin = build_message({response, Message#envelope.id}, <<"control">>, ConnectRep),
	SendRes = ssl:send(SSLSocket, OutBin),
	?debug("Connect response ssl:send result:  ~p", [SendRes]),
	?info("TCPSocket, UDPSocket: ~p, ~p", [TCPSocket, UDPSocket]),
	case {TCPSocket, UDPSocket} of
		{undefined, _} ->
			{ok, State};
		{_, undefined} ->
			{ok, State};
		_ ->
			{ok, ChannelMgr} = pre_client_channels:start_link(),
			NewClientInfo = ClientInfo#client_info{
				channel_manager = ChannelMgr
			},
			NewState = State#state{
				channel_mgr = ChannelMgr,
				client_info = NewClientInfo
			},
			?info("Started pre_client_channels at ~p", [ChannelMgr]),
			pre_hooks:async_trigger_hooks(client_logged_in, [NewClientInfo], all),
			{ok, NewState}
	end.

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

json_encode_decode_test_() -> [
	{"json_to_envelope, simple success", fun() ->
		Expected = #envelope{type = request, channel = <<"goober chan">>},
		Out = json_to_envelope({struct, [{<<"type">>, <<"request">>},
			{<<"channel">>, <<"goober chan">>}]}),
		?assertEqual(Expected, Out)
	end},

	{"json_to_envelope, explosion", fun() ->
		Json = <<"\"not valid json\"">>,
		?assertError({case_clause, _}, json_to_envelope(Json))
	end},

	{"envelope_to_json, simple success", fun() ->
		Expected = lists:sort([{<<"id">>, <<"an id">>},
			{<<"channel">>, <<"goober chan">>}, {<<"type">>, request},
			{<<"contents">>, <<"this is a string">>}]),
		Input = #envelope{type = request, channel = <<"goober chan">>,
			id = <<"an id">>, contents = <<"this is a string">>},
		{struct, Out} = envelope_to_json(Input),
		Out0 = lists:sort(Out),
		?assertEqual(Expected, Out0)
	end}
	].
-endif.
