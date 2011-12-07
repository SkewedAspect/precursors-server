-module(pre_tcp_transient).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("log.hrl").
-include("pre_client.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Socket) ->
	{ok, Socket}.

handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Packet}, Socket) ->
	case netstring:decode(Packet) of
		{[Bin | Tail], Cont} ->
			QH = qlc:q([X || #client_connection{tcp_socket = Bin} = X <- ets:table(client_ets)]),
			[#client_connection{pid = Client}] = qlc:e(QH),
			gen_tcp:controlling_process(Socket, Client),
			pre_client_connection:set_tcp(Client#client_connection.pid, Socket, Tail, Cont),
			{stop, normal, Socket};
		_ ->
			{stop, invalid_cookie, Socket}
	end;

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

