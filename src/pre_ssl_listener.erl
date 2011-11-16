-module(pre_ssl_listener).
-behavior(gen_server).

-include("log.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
	code_change/3]).
-export([start/0,start/1,start_link/0,start_link/1]).
% exported only for internal use
-export([spawn_acceptor/1]).

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
	process_flag(trap_exit, true),
	Port = proplists:get_value(port, Args, 6006),
	SimpleOpts = [list, {packet, line}, {reuseaddr, true},
		{keepalive, true}, {backlog, 30}, {active, false}],
	case ssl:listen(Port, SimpleOpts) of
		{ok, Listen_socket} ->
			%%Create first accepting process
			Acceptor = spawn_link(?MODULE, spawn_acceptor, [Listen_socket]),
			?info("Started on port ~p", [Port]),
			{ok, #state{listener = Listen_socket, acceptor = Acceptor}};
		{error, Reason} ->
			?warning("Could not start ssl:  ~p", [Reason]),
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

handle_info({'EXIT', Pid, Cause}, #state{acceptor = Pid} = State) ->
	Acceptor = spawn_link(?MODULE, spawn_acceptor, [State#state.listener]),
	{noreply, State#state{acceptor = Acceptor}};

handle_info(Req, State) ->
	?debug("Unhandled info:  ~p", [Req]),
	{noreply, State}.

%% =====
%% terminate
%% =====

terminate(Cause, _State) when Cause =:= normal; Cause =:= shutdown ->
	ok;
terminate(Cause, _State) ->
	?warning("ssl listener died due to ~p", [Cause]),
	ok.

%% =====
%% code_change
%% =====

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% =====
%% Internal 
%% =====

spawn_acceptor(Socket) ->
	case ssl:transport_accept(Socket) of
		{ok, NewSocket} ->
			case ssl:ssl_accept(NewSocket, 3000) of
				ok ->
					{ok, Pid} = pre_client_manager:start_client(NewSocket),
					ssl:controlling_process(NewSocket, Pid);
				{error, Reason} ->
					?notice("Ssl connect did not cmplete:  ~p", [Reason])
			end;
		{error, Reason} ->
			?notice("ssl transport accept errored:  ~p", [Reason])
	end.
