-module(pre_client_connection).
-behaviour(gen_server).

-include("log.hrl").
-include("pre_client.hrl").

-record(state, {
	cookie :: binary(),
	ssl_socket,
	ssl_netstring,
	tcp_socket,
	tcp_netstring,
	udp_socket,
	udp_remote_info :: binary() | {string(), integer()} | 'undefined',
	aes_key :: binary(),
	aes_vector :: binary()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,start/2,udp/2,set_tcp/4,send/3,send_tcp/2,send_ssl/2,
	send_udp/2,json_to_envelope/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Socket, Cookie) ->
  gen_server:start_link(?MODULE, {Socket, Cookie}, []).

start(Socket, Cookie) ->
	gen_server:start(?MODULE, {Socket, Cookie}, {}).

udp(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

set_tcp(Pid, Socket, Bins, Cont) ->
	gen_server:cast(Pid, {set_tcp, Socket, Bins, Cont}).

send(Pid, Socket, Binary) when Socket == udp; Socket == ssl; Socket == tcp ->
	gen_server:cast(Pid, {send, Socket, Binary}).

send_tcp(Pid, Binary) -> send(Pid, tcp, Binary).

send_udp(Pid, Binary) -> send(Pid, udp, Binary).

send_ssl(Pid, Binary) -> send(Pid, ssl, Binary).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Socket, Cookie}) ->
	?info("new client connection"),
	ssl:setopts(Socket, [{active, once}]),
	{ok, Udp} = gen_udp:open(0, [{active, once}, binary, {ip, {0,0,0,0}}]),
	State = #state{
		ssl_socket = Socket,
		cookie = Cookie,
		udp_socket = Udp
	},
  {ok, State}.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

%% ------------------------------------------------------------------
%% handle_cast
%% ------------------------------------------------------------------

handle_cast({send, tcp, _Binary}, #state{tcp_socket = undefined} = State) ->
	{noreply, State};

handle_cast({send, tcp, Binary}, State) ->
	Bin = netstring:encode(Binary),
	gen_tcp:send(State#state.tcp_socket, Bin),
	{noreply, State};

handle_cast({send, ssl, Binary}, State) ->
	#state{ssl_socket = Socket} = State,
	Bin = netstring:encode(Binary),
	ssl:send(Socket, Bin),
	{noreply, State};

handle_cast({send, udp, Binary}, #state{udp_remote_info = undefined} = State) ->
	?warning("Tried to send message over UDP before getting UDP sync info: ~p", Binary),
	{noreply, State};

handle_cast({send, udp, Binary}, State) ->
	#state{udp_socket = Sock, udp_remote_info = {Ip, Port}} = State,
	gen_udp:send(Sock, Ip, Port, Binary),
	{noreply, State};
	
handle_cast({set_tcp, Socket, Bins, Cont}, State) ->
	State1 = State#state{tcp_socket = Socket, tcp_netstring = Cont},
	NewState = service_requests(Bins, State1),
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
	?info("tcp socket set:  ~p", [Socket]),
	{noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

handle_info({ssl, Socket, Packet}, #state{ssl_socket = Socket} = State) ->
	{Bins, SSLNetstring} = case State#state.ssl_netstring of
		undefined ->
			netstring:decode(Packet);
		Cont ->
			netstring:decode(Packet, Cont)
	end,
	State1 = State#state{ssl_netstring = SSLNetstring},
	NewState = service_requests(Bins, State1),
	ssl:setopts(Socket, [{active, once}, binary]),
	{noreply, NewState};

handle_info({ssl_closed, Socket}, #state{ssl_socket = Socket} = State) ->
	?info("got ssl socket closed; Imma die"),
	{stop, normal, State};

handle_info({tcp, Socket, Packet}, #state{tcp_socket = Socket} = State) ->
	#state{tcp_netstring = Cont} = State,
	{Bins, Cont1} = netstring:decode(Packet, Cont),
	State1 = State#state{tcp_netstring = Cont1},
	NewState = service_requests(Bins, State1),
	inet:setopts(Socket, [{active, once}]),
	{noreply, NewState};

handle_info({udp, Socket, Ip, InPortNo, Packet},
	#state{udp_socket = Socket, udp_remote_info = undefined} = State) ->
		Success = handle_connect_message(Packet, State),
		case Success of
			ok ->
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
				% Record the UDP information in the state
				State0 = State#state{udp_remote_info = {Ip, InPortNo}},
				?info("UDP port sync up:  ~p:~p", [Ip, InPortNo]),
				inet:setopts(Socket, [{active, once}]),
				{noreply, State0};
			Else ->
				?info("UDP not confirmed:  ~p", [Else]),
				inet:setopts(Socket, [{active, once}]),
				{noreply, State}
		end;

handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{udp_socket = Socket,
	udp_remote_info = {Ip, InPortNo}} = State) ->
		?info("Client got udp:  ~p", [Packet]),
		inet:setopts(Socket, [{active, once}]),
		{noreply, State};

handle_info(Info, State) ->
	?debug("unhandled info:  ~p (~p)", [Info, State]),
	{noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

service_requests([], State) ->
	State;
service_requests([Binary | Tail], State) ->
	NewState = service_request(Binary, State),
	service_requests(Tail, NewState).

%% ------------------------------------------------------------------

service_request(Binary, State) when is_binary(Binary) ->
	Json = mochijson2:decode(Binary),
	Rec = json_to_envelope(Json),
	service_request(Rec, State);

service_request(#envelope{channel = <<"control">>} = Envelope, State) ->
	service_control_channel(Envelope, State);

service_request(#envelope{} = Envelope, State) ->
	?warning("Unhandled reqeust:  ~p", [Envelope]),
	State;

service_request(Request, State) ->
	?warning("Unhandled request with invalid envelope:  ~p", [Request]),
	State.

%% ------------------------------------------------------------------

service_control_channel(#envelope{id = Id, contents = {struct, Request}}, State) ->
	ReqType = proplists:get_value(<<"type">>, Request),
	service_control_channel_request(Id, ReqType, Request, State);

service_control_channel(Thing, State) ->
	?warning("unhandled input:  ~p", [Thing]),
	State.

%% ------------------------------------------------------------------

service_control_channel_request(Id, <<"login">>, Request, State) ->
	?info("starting authentication"),
	% TODO actually ask an authentication system if they should be let in.
	% Send login response
	#state{cookie = Cookie} = State,
	#state{udp_socket = UdpSocket} = State,
	{ok, UdpPort} = inet:port(UdpSocket),
	LoginRep = {struct, [
		{confirm, true},
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
		tcp_socket = Cookie,
		udp_remote_info = undefined,
		aes_key = AESKey,
		aes_vector = AESVector
	}.

wrap_for_send(Recthing) ->
	Json = envelope_to_json(Recthing),
	JsonEnc = mochijson2:encode(Json),
	Binary = list_to_binary(lists:flatten(JsonEnc)),
	netstring:encode(Binary).

%% ------------------------------------------------------------------

envelope_to_json(Envelope) ->
	Props = [{<<"type">>, Envelope#envelope.type},
		{<<"channel">>, Envelope#envelope.channel},
		{<<"contents">>, Envelope#envelope.contents}
	],
	case Envelope#envelope.id of
		undefined -> {struct, Props};
		Id -> {struct, [{<<"id">>, Id} | Props]}
	end.

json_to_envelope(Json) when is_binary(Json) ->
	json_to_envelope(mochijson2:decode(Json));

json_to_envelope({struct, Props}) ->
	Type = proplists:get_value(<<"type">>, Props, <<>>),
	Type0 = check_envelope_type(Type),
	Id = proplists:get_value(<<"id">>, Props),
	Content = proplists:get_value(<<"contents">>, Props),
	Channel = proplists:get_value(<<"channel">>, Props),
	#envelope{type = Type0, id = Id, contents = Content, channel = Channel}.

check_envelope_type(<<"request">>) -> request;
check_envelope_type(<<"response">>) -> response;
check_envelope_type(<<"event">>) -> event.

%% ------------------------------------------------------------------

% Decrypt message
aes_decrypt(Packet, #state{aes_key = AESKey, aes_vector = AESVector}) ->
	% Decrypt message
	Decrypted = crypto:aes_cbc_128_decrypt(AESKey, AESVector, Packet),
	% Strip padding added by OpenSSL's AES algorithm.
	PaddingByteCount = binary:last(Decrypted),
	binary:part(Decrypted, 0, byte_size(Decrypted) - PaddingByteCount).

% Decode JSON message
decode_envelope(Packet) ->
	Json = mochijson2:decode(Packet),
	json_to_envelope(Json).

aes_decrypt_envelope(Packet, State) ->
	decode_envelope(aes_decrypt(Packet, State)).

%% ------------------------------------------------------------------

handle_connect_message(Packet, State) ->
	#state{cookie = Cookie} = State,
	try begin
		% Decrypt message envelope
		Rec = aes_decrypt_envelope(Packet, State),
		#envelope{type = request, channel = <<"control">>, id = MsgID, contents = {struct, Request}} = Rec,
		case proplists:get_value(<<"type">>, Request) of
			<<"connect">> ->
				case proplists:get_value(<<"cookie">>, Request) of
					Cookie -> ok;
					_ -> badcookie
				end;
			_ -> badrequest
		end end,
		% Send response over SSL transport
		ConnectRep = {struct, [
			{confirm, true}
		]},
		Response = #envelope{type = response, channel = <<"control">>, id = MsgID, contents = ConnectRep},
		OutBin = wrap_for_send(Response),
		SendRes = ssl:send(State#state.ssl_socket, OutBin),
		?debug("Connect response ssl:send result:  ~p", [SendRes])
	catch
		What:Why ->
			{What,Why}
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
