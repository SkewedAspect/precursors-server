-module(pre_client_connection).
-behaviour(gen_server).

-include("log.hrl").
-include("precursors_pb.hrl").
-include("pre_client.hrl").

-record(state, {
	cookie :: binary(),
	ssl_socket,
	ssl_netstring,
	tcp_socket,
	tcp_netstring,
	udp_socket,
	udp_remote_info :: binary() | {string(), integer()} | 'undefined'
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,start/2,udp/2,set_tcp/4,send/3,send_tcp/2,send_ssl/2,
	send_udp/2]).

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

handle_cast({send, udp, _Binary}, #state{udp_remote_info = undefined} = State) ->
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

handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{udp_socket = Socket,
	udp_remote_info = undefined} = State) ->
		#state{cookie = Cookie} = State,
		Success = try begin
			Rec = precursors_pb:decode_envelope(Packet),
			#envelope{channel = "control", event = Event} = Rec,
			#event{connect_port = #connectport{cookie = TestCookie}} = Event,
			case list_to_binary(TestCookie) of
				Cookie -> ok;
				_ -> badcookie
			end end
		catch
			What:Why ->
				{What,Why}
		end,
		case Success of
			ok ->
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
				State0 = State#state{udp_remote_info = {Ip, InPortNo}},
				?info("Udp port sync up:  ~p:~p", [Ip, InPortNo]),
				inet:setopts(Socket, [{active, once}]),
				{noreply, State0};
			Else ->
				?info("Udp not confirmed:  ~p", [Else]),
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
	Rec = precursors_pb:decode_envelope(Binary),
	service_request(Rec, State);

service_request(#envelope{channel = "control"} = Envelope, State) ->
	service_control_channel(Envelope, State);

service_request(Request, State) ->
	?warning("Unhandled reqeust:  ~p", [Request]),
	State.

%% ------------------------------------------------------------------

service_control_channel(#envelope{request = Request}, State) ->
	service_control_channel_request(Request, State);

service_control_channel(Thing, State) ->
	?warning("unhandled input:  ~p", [Thing]),
	State.

%% ------------------------------------------------------------------

service_control_channel_request(#request{login = Login} = Request, State) when is_record(Login, login) ->
	?info("starting authentication"),
	% TODO actually ask an authentication system if they should be let in.
	#request{id = ReqId} = Request,
	#state{cookie = Cookie} = State,
	%Cookie = lists:flatten(io_lib:format("~p", [erlang:make_ref()])),
	#state{udp_socket = UdpSocket} = State,
	{ok, UdpPort} = inet:port(UdpSocket),
	LoginRep = #loginreply{
		cookie = Cookie,
		udp_port = UdpPort,
		tcp_port = 6007,
		channels = [#channel{channel_name = "control", connection_type = 'SSL'}]
	},
	Response = #response{
		inresponseto = ReqId,
		confirm = true,
		login = LoginRep
	},
	OutBin = wrap_for_send("control", Response),
	SendRes = ssl:send(State#state.ssl_socket, OutBin),
	?debug("authentiation ssl:send result:  ~p", [SendRes]),
	State#state{cookie = Cookie, tcp_socket = Cookie,
		udp_remote_info = undefined
	}.

wrap_for_send(Channel, Recthing) ->
	Base = #envelope{channel = Channel},
	OutRec = case Recthing of
		X when is_record(X, response) ->
			Base#envelope{response = X};
		X when is_record(X, request) ->
			Base#envelope{request = X};
		X when is_record(X, event) ->
			Base#envelope{event = X}
	end,
	netstring:encode(precursors_pb:encode(OutRec)).
