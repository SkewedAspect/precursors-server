%%% @doc The client's connection process.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_client).

-behaviour(gen_server).

-include("pre_client.hrl").

% API
-export([start_link/2,
	handle_messages/3,
	handle_message/3,
	send_event/3,
	send_response/4,
	send_request/3
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

handle_messages(ClientPid, Transport, Messages) ->
	[handle_message(ClientPid, Transport, Message) || Message <- Messages],
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

handle_message(ClientPid, Transport, Message) ->
	gen_server:cast(ClientPid, {Transport, Message}).

%% ---------------------------------------------------------------------------------------------------------------------

send_event(ClientPid, Channel, Content) ->
	gen_server:cast(ClientPid, { send_event, Channel, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

send_response(ClientPid, Channel, ID, Content) ->
	gen_server:cast(ClientPid, { send_response, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------

send_request(ClientPid, Channel, ID, Content) ->
	gen_server:cast(ClientPid, { send_request, Channel, ID, Content }).

%% ---------------------------------------------------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------------------------------------------------

init([SslProto, Cookie]) ->
	{ok, #state{ssl_proto = SslProto, cookie = Cookie}}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_call(Request, _From, State) ->
	lager:debug("Unhandled call:  ~p", [Request]),
	{reply, unknown, State}.

%% ---------------------------------------------------------------------------------------------------------------------

handle_cast({ssl, Message}, State) ->
	State1 = process_channel(Message, State),
	{noreply, State1};

handle_cast({tcp, #envelope{channel = <<"control">>} = Message}, State) ->
	%TODO: Do we want to drop the client? What about logging more information?
	lager:error("Control message received over TCP! Someone's doing something bad! Message: ~p", [Message]),
	{noreply, State};

handle_cast({tcp, Message}, State) ->
	State1 = process_channel(Message, State),
	{noreply, State1};

%% @hidden
handle_cast(Request, State) ->
	lager:debug("Unhandled cast:  ~p", [Request]),
	{noreply, State}.

%% ---------------------------------------------------------------------------------------------------------------------

handle_info(Info, State) ->
	lager:warning("Unhandled message: ~p", [Info]),
	{noreply, State}.

%% ---------------------------------------------------------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%% ---------------------------------------------------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------------------------------

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

