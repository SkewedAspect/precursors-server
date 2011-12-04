-module(pre_client_connection).
-behaviour(gen_server).

-include("log.hrl").

-record(state, {
	cookie :: binary(),
	ssl_socket,
	tcp_socket,
	udp_socket,
	udp_remote_info :: binary() | {string(), integer()} | 'undefined'
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,start/2,udp/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Socket, Cookie) ->
  gen_server:start_link(?MODULE, {Socket, Cookie}, []).

start(Socket, Cookie) ->
	gen_server:start(?MODULE, {Socket, Cookie}, {}).

udp(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

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

handle_cast(_Msg, State) ->
  {noreply, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

%handle_info({tcp, Socket, Packet}, #state{ssl_socket = Socket} = State) ->
%	?debug("got ssl socket packet while in tcp:  ~p", [Packet]),
%	inet:setopts(Socket, [{active, once}]),
%	{noreply, State};
%
%handle_info({tcp_closed, Socket}, #state{ssl_socket = Socket} = State) ->
%	?info("got ssl socket closed while in tcp, Imma die"),
%	{stop, ssl_closed, State};

handle_info({ssl, Socket, Packet}, #state{ssl_socket = Socket} = State) ->
	?debug("got ssl packet:  ~p", [Packet]),
	ssl:send(Socket, Packet),
	ssl:setopts(Socket, [{active, once}]),
	{noreply, State};

handle_info({ssl_closed, Socket}, #state{ssl_socket = Socket} = State) ->
	?info("got ssl socket closed; Imma die"),
	{stop, normal, State};

handle_info({udp, Socket, IP, InPortNo, Packet}, #state{udp_socket = Socket,
	udp_remote_info = Packet} = State) ->
		?info("Client confirmed udp port sync up"),
		NewState = State#state{udp_remote_info = {IP, InPortNo}},
		{noreply, NewState};

handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{udp_socket = Socket,
	udp_remote_info = {Ip, InPortNo}} = State) ->
		?info("Client got udp:  ~p", [Packet]),
		{noreply, State};

handle_info(Info, State) ->
	?debug("unhandled info:  ~p", [Info]),
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

