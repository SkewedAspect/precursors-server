%% @doc Isolates a new tcp connection until it is able to identify itself
%% as a valid client.  It then either disconnects, or gives control of the
%% socket to the correct client connection.
-module(pre_tcp_transient).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("pre_channel/include/pre_client.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts linked to the calling process.
-spec(start_link/1 :: (Socket :: any()) -> {'ok', pid()}).
start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

%% @doc Starts unlinked to the calling process.
-spec(start/1 :: (Socket :: any()) -> {'ok', pid()}).
start(Socket) ->
	gen_server:start(?MODULE, Socket, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @hidden
init(Socket) ->
	lager:info("New transient TCP:  ~p", [Socket]),
	{ok, {Socket, 10}, 500}.

%% @hidden
handle_call(Request, From, State) ->
	lager:debug("Unhandled call from ~p:  ~p", [From, Request]),
	{noreply, ok, State}.

%% @hidden
handle_cast(start_accept, {Socket, _}) ->
	inet:setopts(Socket, [{active, once}]),
	{noreply, {Socket, 10}, 5000};

handle_cast(Msg, State) ->
	lager:debug("Unhandled cast:  ~p", [Msg]),
	{noreply, State}.

%% @hidden
handle_info({tcp, Socket, Packet}, {Socket, InCont}) ->
	case netstring:decode(Packet, InCont) of
		{[Binary | Tail], Cont} ->
			% Decode message envelope
			Message = pre_client_connection:json_to_envelope(Binary),
			#envelope{type = request, channel = <<"control">>, contents = Request} = Message,
			Cookie = proplists:get_value(cookie, Request),
			% Look up client PID by cookie in ETS
			lager:debug("Checking for cookie:  ~p", [Cookie]),
			QH = qlc:q([X || #client_connection{tcp_socket = TestCookie} = X <- ets:table(client_ets), TestCookie =:= Cookie]),
			[#client_connection{pid = Client}] = qlc:e(QH),
			% Transfer connection ownership to client
			gen_tcp:controlling_process(Socket, Client),
			pre_client_connection:set_tcp(Client, Socket, Message, Tail, Cont),
			gen_server:cast(Client, start_accept_tcp),
			{stop, normal, Socket};
		{[], Cont} ->
			inet:setopts(Socket, [{active, once}]),
			{noreply, {Socket, Cont}, 5000};
		Abuh ->
			lager:warning("Netstring decode exploded:  ~p", [Abuh]),
			{stop, invalid_cookie, Socket}
	end;

handle_info(timeout, State) ->
	lager:warning("Client (or ancestor) did not respond in time"),
	{stop, timeout, State};

handle_info(Info, State) ->
	lager:debug("Unhandled info:  ~p", [Info]),
	{noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
	ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

