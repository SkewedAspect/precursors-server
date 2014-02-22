%%% @doc This is the Ranch protocol that decodes the messages from TCP/SSL, and turns them into something the server can
%%% understand. This module is in charge of handling the decoding of the netstrings, and the AES encryption, as well as
%%% starting the client connection pid when an SSL connection happens.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_channels_proto).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("pre_client.hrl").

% API
-export([start_link/4]).

% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

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
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Ref, Socket, Transport], Opts).

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
handle_cast(Request, State) ->
	lager:debug("Unhandled cast:  ~p", [Request]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

% Handles incoming SSL data
handle_info({ssl, _Socket, Data}, State) ->
	lager:warning("Incoming SSL message: ~p", [Data]),

	{Messages, Cont} = netstring:decode(data, State#state.netstring_cont),
	State1 = State#state{netstring_cont = Cont},

  DecodedMessages = [json_to_envelope(Message) || Message <- Messages],

	%TODO: Send the messages to the client connection pid.

	{noreply, State1};

% Handles incoming TCP data
handle_info({tcp, _Socket, Data}, State) ->
	lager:warning("Incoming TCP message: ~p", [Data]),

	{Messages, Cont} = netstring:decode(data, State#state.netstring_cont),
	State1 = State#state{netstring_cont = Cont},

	DecryptedMessages = [aes_decrypt_envelope(Message, State1) || Message <- Messages],

	%TODO: Send the messages to the client connection pid.

	{noreply, State1};

% Sets up the ranch protocol.
handle_info(timeout, State=#state{ref = Ref, socket = Socket, transport = Transport}) ->
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),

	State1 = case Socket of
		ssl ->
			%TODO: spawn the client connection pid.
			State;
		tcp ->
			%TODO: look up the client connection pid, (and retrieve the AES key/vector?)
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

