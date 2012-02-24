-module(pre_tcp_listener).
-behavior(gen_server).

-include("log.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
	code_change/3]).
-export([start/0,start/1,start_link/0,start_link/1,spawn_acceptor/1]).

-record(state, {
	listener,
	acceptors = []
}).

%% =====
%% API
%% =====
start() -> start([]).

start(Args) -> gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link() -> start_link([]).

start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% =====
%% init
%% =====

init(Args) ->
	process_flag(trap_exit, true),
	Port = proplists:get_value(port, Args, 6007),
	SimpleOpts = [list, {packet, raw}, {reuseaddr, true}, binary,
		{keepalive, true}, {backlog, 30}, {active, false}],
	case gen_tcp:listen(Port, SimpleOpts) of
		{ok, Listen_socket} ->
			AcceptNum = proplists:get_value(poolsize, Args, 5),
			Acceptors = spawn_acceptors(Listen_socket, AcceptNum),
			?info("Started on port ~p", [Port]),
			{ok, #state{listener = Listen_socket, acceptors = Acceptors}};
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

handle_info({'EXIT', Pid, _Reason}, State) ->
	?debug("tcp acceptor died due to ~p", [Reason]),
	#state{acceptors = Acceptors, listener = Sock} = State,
	Acceptors0 = lists:delete(Pid, Acceptors),
	NewPid = spawn_link(?MODULE, spawn_acceptor, [Sock]),
	Acceptors1 = [NewPid | Acceptors0],
	{noreply, State#state{acceptors = Acceptors1}};
	
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

%% =====
%% internal
%% =====

spawn_acceptors(Sock, Num) ->
	spawn_acceptors(Sock, Num, []).

spawn_acceptors(_Sock, 0, Acc) ->
	Acc;

spawn_acceptors(Sock, Num, Acc) when Num > 0 ->
	Pid = spawn_link(?MODULE, spawn_acceptor, [Sock]),
	spawn_acceptors(Sock, Num - 1, [Pid | Acc]).

spawn_acceptor(Sock) ->
	case gen_tcp:accept(Sock) of
		{ok, Sock} ->
			{ok, Pid} = pre_tcp_transient:start(Sock),
			ok = gen_tcp:controlling_process(Sock, Pid),
			gen_server:cast(Pid, start_accept),
			exit(normal);
		Else ->
			exit(Else)
	end.
