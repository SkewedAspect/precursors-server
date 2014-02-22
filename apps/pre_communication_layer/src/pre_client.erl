%%% @doc
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_client).

-behaviour(gen_server).

-include("pre_client.hrl").

% API
-export([start_link/0, start_link/1, handle_messages/3, handle_message/3]).

% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	ssl_proto :: pid(),
	tcp_proto :: pid(),
	aes_key :: binary(),
	aes_vector :: binary()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link() ->
	start_link([]).

start_link(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% --------------------------------------------------------------------------------------------------------------------

handle_messages(ClientPid, Transport, Messages) ->
	[handle_message(ClientPid, Transport, Message) || Message <- Messages],
	ok.

%% --------------------------------------------------------------------------------------------------------------------

handle_message(ClientPid, Transport, Message) ->
	gen_server:cast(ClientPid, {Transport, Message}).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init(_Args) ->
	{ok, #state{}}.

%% --------------------------------------------------------------------------------------------------------------------

%% @hidden
handle_call(Request, _From, State) ->
	lager:debug("Unhandled call:  ~p", [Request]),
	{reply, unknown, State}.

%% --------------------------------------------------------------------------------------------------------------------

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

%% --------------------------------------------------------------------------------------------------------------------

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

process_channel(#envelope{channel = <<"control">>} = Message, State) ->
	lager:info("Control channel message: ~p", [Message]),
	State;

process_channel(Message, State) ->
	lager:warning("Got message for unknown channel: ~p", [Message]),
	State.

%% --------------------------------------------------------------------------------------------------------------------

process_message({control, Message}, State) ->
	State.