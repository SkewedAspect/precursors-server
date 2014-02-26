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
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Ref, Socket, Transport], Opts).

%% --------------------------------------------------------------------------------------------------------------------

send_envelope(ProtoPid, envelope) ->
	gen_server:cast(ProtoPid, {send, envelope}).

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
	{ok, {state, Ref, Socket, Transport}, 0}.

%% --------------------------------------------------------------------------------------------------------------------
%% @hidden

handle_call(Request, _From, State) ->
	lager:debug("Unhandled call:  ~p", [Request]),
	{reply, unknown, State}.

%% --------------------------------------------------------------------------------------------------------------------
%% @hidden

handle_cast({send, Envelope}, State) ->
	% If we have an aes key, we assume we need to aes encrypt the message.
	Data = case State#state.aes_key of
		undefined ->
			pre_json:to_json(Envelope);
		_ ->
			% Encrypt the envelope
			aes_encrypt(pre_json:to_json(Envelope), State#state.aes_key, State#state.aes_vector)
	end,

	Packet = netstring:encode(Data),

	Transport = State#state.transport,
	Socket = State#state.socket,
	Transport:send(Socket, Packet),

	{noreply, State};


handle_cast(Request, State) ->
	lager:debug("Unhandled cast:  ~p", [Request]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------
%% @hidden

% Handles incoming SSL data
handle_info({ssl, _Socket, Data}, State) ->
	lager:warning("Incoming SSL message: ~p", [Data]),

	{Messages, Cont} = netstring:decode(data, State#state.netstring_cont),
	State1 = State#state{netstring_cont = Cont},

	DecodedMessages = [json_to_envelope(Message) || Message <- Messages],

	% Send the messages to the client connection pid.
	pre_client:handle_message(State#state.client, ssl, DecodedMessages),

	{noreply, State1};


% Handles incoming TCP data
handle_info({tcp, _Socket, Data}, State) ->
	lager:warning("Incoming TCP message: ~p", [Data]),

	{Messages, Cont} = netstring:decode(data, State#state.netstring_cont),
	State1 = State#state{netstring_cont = Cont},

	% Check to see if we have our client yet. If not, we look through the messages for a cookie. (Om nom nom.)
	State2 = case State1#state.client of
		undefined ->
			check_for_cookie(Messages, State1);
		_ ->
			% Messages are only encrypted if we have found our client pid.
			DecryptedMessages = [aes_decrypt_envelope(Message, State1) || Message <- Messages],

			% Send the messages to the client connection pid.
			pre_client:handle_message(State1#state.client, tcp, DecryptedMessages)
	end,

	{noreply, State2};


% Sets up the ranch protocol.
handle_info(timeout, State=#state{ref = Ref, socket = Socket, transport = Transport}) ->
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),

	State1 = case Socket of
		         ssl ->
			         % Spawn a new client pid
			         pre_client_sup:start_child/1(self()),
			         State;
		         tcp ->
			         State;
		         Sock ->
			         lager:warning("Unknown Socket Type: ~p", [Sock]),
			         State
	         end,

	{noreply, State1};


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
			pre_client:handle_message(State#state.client, tcp, Rest),
			State2;
		{false, _} ->
			check_for_cookie(Rest, State)
	end,
	State1;

check_for_cookie(Message, State) ->
	State1 = case Message of
		#envelope{type = request, channel = <<"control">>, contents = Request} ->
			Cookie = proplists:get_value(cookie, Request),
			ClientPid = pre_client:connect_tcp(self(), Cookie),
			{AESKey, AESVec} = pre_client:get_aes(ClientPid),
			State#state{ client = ClientPid, aes_key = AESKey, aes_vector = AESVec };
		_ ->
			%TODO: Kill the connection.
			lager:warning("Non-cookie message received: ~p, ~p, ~p",
				[Message#envelope.channel, Message#envelope.type, Message#envelope.contents])
	end,

	{false, State1}.

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
aes_decrypt(CipherText, #state{aes_key = AESKey, aes_vector = AESVector}) ->
	Decrypted = crypto:block_decrypt(aes_cbc128, AESKey, AESVector, CipherText),

	% Strip padding added by OpenSSL's AES algorithm.
	PaddingByteCount = binary:last(Decrypted),
	binary:part(Decrypted, 0, byte_size(Decrypted) - PaddingByteCount).

% Decode JSON message
aes_decrypt_envelope(Packet, State) ->
	json_to_envelope(aes_decrypt(Packet, State)).

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
