%%% @doc The client's connection process.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_client).

-behaviour(gen_server).

-include("pre_client.hrl").

% API
-export([start_link/2,
	new/1,
	connect_tcp/2,
	get_aes / 1,
	handle_messages/3,
	handle_message/3,
	send_event/3,
	send_response/4,
	send_request/4
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
	gen_server:start_link({local, ?SERVER}, ?MODULE, [SslProto, Cookie], []).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Starts a new client connection process. You should always use this method to create a new client pid.
-spec new(SslProto :: pid()) -> ok.

new(SslProto) ->
	Cookie = make_bin_ref(),

	% Ask the supervisor to create a new client connection pid.
	pre_client_sup:start_child(SslProto, Cookie),
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Looks up the client pid by Cookie, and informs that pid of the `TcpProto` pid. Then deletes the entry in the ets
%% table. (This helps keep the table nice and small.)
-spec connect_tcp(TcpProto :: pid(), Cookie :: binary()) -> ClientPid :: pid().

connect_tcp(TcpProto, Cookie) ->
	[{_, ClientPid}] = ets:lookup(client_ets, Cookie),
	gen_server:cast(ClientPid, {tcp_connect, TcpProto}),

	% Clean ourselves up.
	ets:delete(client_ets, Cookie),
	ClientPid.

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

%% @doc Send an event to the client. `Content' must be able JSON serializable. (Note: this method is responsible for
%% wrapping `Content' in the envelope structure.)
-spec send_event(ClientPid :: pid(), Channel :: binary(), Content :: term()) -> ok.

send_event(ClientPid, Channel, Content) ->
	gen_server:cast(ClientPid, { send_event, Channel, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Send a response to the client. `Content' must be able JSON serializable. (Note: this method is responsible for
%% wrapping `Content' in the envelope structure.)
-spec send_response(ClientPid :: pid(), Channel :: binary(), ID :: any(), Content :: term()) -> ok.

send_response(ClientPid, Channel, ID, Content) ->
	gen_server:cast(ClientPid, { send_response, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Send a request to the client. `Content' must be able JSON serializable. (Note: this method is responsible for
%% wrapping `Content' in the envelope structure.) !!! Currently, there is never any reason to use this method. !!!
-spec send_request(ClientPid :: pid(), Channel :: binary(), ID :: any(), Content :: term()) -> ok.

send_request(ClientPid, Channel, ID, Content) ->
	gen_server:cast(ClientPid, { send_request, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------------------------------------------------

init([SslProto, Cookie]) ->
	% Add ourselves to the ets table, by cookie
	ets:insert(client_ets, {Cookie, self()}),

	% Wait a maximum of 30 seconds for the tcp connection.
	erlang:send_after(30000, self(), check_tcp),

	{ok, #client_state{ssl_proto = SslProto, cookie = Cookie}}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_call(get_aes, _From, State) ->
	%TODO: Check `From' and make sure it's our TCPProto?
	{reply, { State#client_state.aes_key, State#client_state.aes_vector }, State};

%% @hidden
handle_call(Request, _From, State) ->
	lager:debug("Unhandled call:  ~p", [Request]),
	{reply, unknown, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_cast({tcp_connect, TcpProto}, State) ->
	State1 = State#client_state{tcp_proto = TcpProto},
	{noreply, State1};

%% @hidden
handle_cast({ssl, Message}, State) ->
	State1 = process_channel(Message, State),
	{noreply, State1};

%% @hidden
handle_cast({tcp, #envelope{channel = <<"control">>} = Message}, State) ->
	%TODO: Do we want to drop the client? What about logging more information?
	lager:error("Control message received over TCP! Someone's doing something bad! Message: ~p", [Message]),
	{noreply, State};

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
			%TODO: Inform the client, somehow, of the reason.
			exit("TCP failed to connect after 30 seconds.");
		_ ->
			ok
	end,
	{noreply, State};

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
			lager:info("Client disconnected.");
		_ ->
			lager:warning("Client disconnected unexpectedly! Reason: ~p", [Reason])
	end,

	ok.

%% ---------------------------------------------------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden

% Send the message off to a callback module.
process_channel(#envelope{channel = <<"control">>} = Message, State) ->
	Contents = Message#envelope.contents,
	ReqType = proplists:get_value(type, Contents),

	State1 = case Message#envelope.type of
		 <<"event">> ->
			 pre_control_channel:handle_event({ ReqType, Message#envelope.id, Contents }, State);
		 <<"request">> ->
			 pre_control_channel:handle_request({ ReqType, Message#envelope.id, Contents }, State);
		 <<"response">> ->
			 pre_control_channel:handle_response({ ReqType, Message#envelope.id, Contents }, State);
		 _ ->
			 lager:warning("Unknown Message Type: ~p", [Message])
	end,
	State1;

process_channel(Message, State) ->
	lager:warning("Got message for unknown channel: ~p", [Message]),
	State.

% Make a ref and then wrap it in a bin
make_bin_ref() ->
	list_to_binary(io_lib:format("~p", [make_ref()])).

