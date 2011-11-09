-module(pre_tcp_listener).
-behavior(gen_server).

-include("log.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
	code_change/3]).
-export([start/0,start/1,start_link/0,start_link/1]).

-record(state, {
	listener,
	acceptor
}).

%% =====
%% API
%% =====
start() -> start([]).

start(Args) -> gen_server:start(?MODULE, Args, []).

start_link() -> start_link([]).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

%% =====
%% init
%% =====

init(Args) ->
	Port = proplists:get_value(port, Args, 6007),
	SimpleOpts = [list, {packet, line}, {reuseaddr, true},
		{keepalive, true}, {backlog, 30}, {active, false}],
	case gen_tcp:listen(Port, SimpleOpts) of
		{ok, Listen_socket} ->
			%%Create first accepting process
			{ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
			?info("Started on port ~p", [Port]),
			{ok, #state{listener = Listen_socket, acceptor = Ref}};
		{error, Reason} ->
			?warning("Could not start gen_tcp:  ~p", [Reason]),
			{stop, Reason}
	end.

%% =====
%% handle_call
%% =====

handle_call(Req, _From, State) ->
	?debug("Unhandled call:  ~p", [Req]),
	{reply, unknown, State}.

%% =====
%% handle_cast
%% =====

handle_cast(Req, State) ->
	?debug("Unhandled cast:  ~p", [Req]),
	{noreply, State}.

%% =====
%% handle_info
%% =====

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listener=ListSock, acceptor=Ref} = State) ->
	try inet:setopts(State#state.listener, [{active, once}]) of
		ok  ->
			%% New client connected
			{ok, Pid} = pre_client_manager:start_client(CliSocket),
			gen_tcp:controlling_process(CliSocket, Pid),
			case prim_inet:async_accept(ListSock, -1) of
				{ok, NewRef} ->
					{noreply, State#state{acceptor = NewRef}};
				{error, NewRef} ->
					{stop, {async_accept, inet:format_error(NewRef)}, State}
			end;
		{error, Reason} ->
			{sopt, {set_sockopt, Reason}, State}
	catch
		exit:Why ->
			error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
			{stop, Why, State}
	end;

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
	?warning("Error in socket acceptor: ~p", [Error]),
	{stop, Error, State};

handle_info(Req, State) ->
	?debug("Unhandled info:  ~p", [Req]),
	{noreply, State}.

%% =====
%% terminate
%% =====

terminate(Cause, _State) when Cause =:= normal; Cause =:= shutdown ->
	ok;
terminate(Cause, _State) ->
	?warning("tcp listener died due to ~p", [Cause]),
	ok.

%% =====
%% code_change
%% =====

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
