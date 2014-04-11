%%% @doc This is the Ranch protocol that decodes the messages from TCP/SSL, and turns them into something the server can
%%% understand. This module is in charge of handling the decoding of the netstrings, and the AES encryption, as well as
%%% starting the client connection pid when an SSL connection happens.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_channels_proto).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("pre_client.hrl").

-define(AES_BLOCK_SIZE, 16).

% API
-export([start_link/4,
	send_envelope/2
]).

% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	ref :: any(),
	socket :: any(),
	transport :: atom(),
	client :: pid(),
	aes_key :: binary(),
	aes_vector :: binary(),
	netstring_cont = 10 :: integer() | term()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts the server

start_link(Ref, Socket, Transport, Opts) ->
	gen_server:start_link(?MODULE, [Ref, Socket, Transport], Opts).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Sends an envelope'd message to the client, over the network.
-spec send_envelope(ProtoPid :: pid(), Envelope :: #envelope{}) -> ok.

send_envelope(ProtoPid, Envelope) ->
	gen_server:cast(ProtoPid, {send, Envelope}).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([]) ->
	lager:debug("Wrong init: not called from ranch?"),
	{ok, #state{}};

init([Ref, Socket, Transport]) ->
	% If you return a timeout value of 0 then the `gen_server' will call `handle_info(timeout, _, _)' right away.
	% We do this to work around issues with `gen_server' and ranch's protocols. See the ranch docs:
	%   http://ninenines.eu/docs/en/ranch/HEAD/guide/protocols/#using_gen_server
	{ok, #state{ref = Ref, socket = Socket, transport = Transport}, 0}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_call(Request, _From, State) ->
	lager:debug("Unhandled call:  ~p", [Request]),
	{reply, unknown, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_cast(stop, State) ->
	{stop, normal, State};

%% @hidden
handle_cast({send, Envelope}, State) ->
	% If we have an aes key, we assume we need to aes encrypt the message.
	Data = case State#state.aes_key of
		undefined ->
			pre_json:to_json(envelope_to_json(Envelope));
		_ ->
			EnvelopeJson = envelope_to_json(Envelope),

			% Encrypt the envelope
			aes_encrypt(pre_json:to_json(EnvelopeJson), State#state.aes_key, State#state.aes_vector)
	end,

	Packet = netstring:encode(Data),

	Transport = State#state.transport,
	Socket = State#state.socket,
	Transport:send(Socket, Packet),

	{noreply, State};


%% @hidden
handle_cast(Request, State) ->
	lager:debug("Unhandled cast:  ~p", [Request]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden Handles incoming SSL data
handle_info({ssl, Socket, Data}, State) ->
	lager:debug("SSL Message: ~p", [Data]),

	{Messages, Cont} = netstring:decode(Data, State#state.netstring_cont),
	State1 = State#state{netstring_cont = Cont},

	DecodedMessages = [json_to_envelope(Message) || Message <- Messages],

	% Send the messages to the client connection pid.
	pre_client:handle_messages(State#state.client, ssl, DecodedMessages),

	% Tell the transport to get the next message
	Transport = State1#state.transport,
	ok = Transport:setopts(Socket, [{active, once}]),

	{noreply, State1};

%% @hidden Handles incoming TCP data
handle_info({tcp, Socket, Data}, State) ->
	lager:debug("TCP Message: ~p", [Data]),

	{Messages, Cont} = netstring:decode(Data, State#state.netstring_cont),
	State1 = State#state{netstring_cont = Cont},

	% Check to see if we have our client yet. If not, we look through the messages for a cookie. (Om nom nom.)
	State2 = case State1#state.client of
		undefined ->
			check_for_cookie(Messages, State1);
		_ ->
			% Messages are only encrypted if we have found our client pid.
			DecryptedMessages = [aes_decrypt_envelope(Message, State1) || Message <- Messages],

			% Send the messages to the client connection pid.
			pre_client:handle_messages(State1#state.client, tcp, DecryptedMessages),
			State1
	end,

	% Tell the transport to get the next message
	Transport = State2#state.transport,
	ok = Transport:setopts(Socket, [{active, once}]),

	{noreply, State2};


%% @hidden Handles the TCP socket closing.
handle_info({tcp_closed, _Socket}, State) ->
	%TODO: Wait for a reconnect, maybe?
	lager:debug("TCP socket closed."),
	{noreply, State};


%% @hidden Handles the SSL socket closing.
handle_info({ssl_closed, _Socket}, State) ->
	Client = State#state.client,
	gen_server:call(Client, {stop_client, ssl_closed}),
	{noreply, State};


%% @hidden Sets up the ranch protocol.
handle_info(timeout, State=#state{ref = Ref, socket = Socket, transport = Transport}) ->
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),

	State1 = case Transport of
		ranch_ssl ->
			% Spawn a new client pid
			{ok, ClientPid} = pre_client_sup:start_child(self()),
			State#state{client = ClientPid};
		ranch_tcp ->
			State
	end,

	{noreply, State1};

%% @hidden
handle_info(Info, State) ->
	lager:warning("Unhandled message: ~p", [Info]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%% --------------------------------------------------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal
%% --------------------------------------------------------------------------------------------------------------------

% Check messages for a cookie
check_for_cookie([], State) ->
	State;

check_for_cookie([Message | Rest], State) ->
	State1 = case check_for_cookie(Message, State) of
		{true, State2} ->
			State2;
		{false, _} ->
			check_for_cookie(Rest, State)
	end,
	State1;

check_for_cookie(Message, State) ->
	Decoded = json_to_envelope(Message),
	case Decoded of
		#envelope{type = request, id = ID, channel = <<"control">>, contents = Request} when is_list(Request) ->
			case proplists:get_value(cookie, Request) of
				undefined ->
					lager:warning("Non-cookie message received: ~p, ~p, ~p",
						[Message#envelope.channel, Message#envelope.type, Message#envelope.contents]),
					exit(non_cookie_message),
					{false, State};
				Cookie ->
					ClientPid = pre_client:connect_tcp(self(), Cookie, ID),

					{AESKey, AESVec} = pre_client:get_aes(ClientPid),
					{true, State#state{ client = ClientPid, aes_key = AESKey, aes_vector = AESVec }}
			end;

		_ ->
			lager:warning("Non-cookie message received: ~p, ~p, ~p",
				[Message#envelope.channel, Message#envelope.type, Message#envelope.contents]),
			exit(non_cookie_message),
			{false, State}
	end.

get_pkcs5_padding(Packet) ->
	PacketLength = byte_size(Packet),
	PaddingNeeded = ?AES_BLOCK_SIZE - (PacketLength rem ?AES_BLOCK_SIZE),
	binary:copy(<<PaddingNeeded/integer>>, PaddingNeeded).

% Encrypt message
aes_encrypt(Packet, AESKey, AESVector) ->
	Padding = get_pkcs5_padding(Packet),
	Padded = <<Packet/binary, Padding/binary>>,
	crypto:aes_cbc_128_encrypt(AESKey, AESVector, Padded).

% Decrypt message
aes_decrypt(CipherText, #state{aes_key = AESKey, aes_vector = AESVector}) ->
	Decrypted = crypto:block_decrypt(aes_cbc128, AESKey, AESVector, CipherText),

	% Strip padding added by OpenSSL's AES algorithm.
	PaddingByteCount = binary:last(Decrypted),
	binary:part(Decrypted, 0, byte_size(Decrypted) - PaddingByteCount).

% Decode JSON message
aes_decrypt_envelope(Packet, State) ->
	json_to_envelope(aes_decrypt(Packet, State)).

% Extract the json message from an envelope record
envelope_to_json(Envelope) ->
	#envelope{
		type = Type,
		channel = Channel,
		contents = Contents,
		id = MessageID
	} = Envelope,
	Props = [
		{type, if is_binary(Type) -> Type; is_atom(Type) -> list_to_binary(atom_to_list(Type)) end},
		{channel, Channel},
		{contents, Contents}
	],
	case MessageID of
		undefined -> Props;
		MessageID -> [{id, MessageID} | Props]
	end.

% Pull the json message into an envelope record
json_to_envelope(Json) when is_binary(Json) ->
	json_to_envelope(pre_json:to_term(Json));

json_to_envelope([{_, _} | _] = Props) ->
	Type = proplists:get_value(type, Props, <<>>),
	Type0 = check_envelope_type(Type),
	MessageID = proplists:get_value(id, Props),
	Contents = proplists:get_value(contents, Props),
	Channel = proplists:get_value(channel, Props),
	#envelope{type = Type0, id = MessageID, contents = Contents, channel = Channel}.

% Super Easy!
check_envelope_type(<<"request">>) -> request;
check_envelope_type(<<"response">>) -> response;
check_envelope_type(<<"event">>) -> event.
