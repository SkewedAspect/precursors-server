%% @doc Ssl Socket listening server.  Clients are expected to first contact
%% this listeer before attempting to reach the others.  Uses an accept 
%% pool for extra awesome.
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
	acceptors,
	poolsize = 5
}).

%% =====
%% Types
%% =====

-type(port_opt() :: {port, pos_integer()}).
-type(certfile_opt() :: {certfile, string()}).
-type(keyfile_opt() :: {keyfile, string()}).
-type(poolsize_opt() :: {poolsize, pos_integer()}).
-type(start_opt() :: port_opt() | certfile_opt() | keyfile_opt() |
		poolsize_opt()).
-type(start_opts() :: [start_opt()]).

%% =====
%% API
%% =====

%% @doc Starts an unlinked ssl listener with default options.
-spec(start/0 :: () -> {'ok', pid()}).
start() -> start([]).

%% @doc Starts an unlinked ssl listener with the given options.
-spec(start/1 :: (Args :: start_opts()) -> {'ok', pid()}).
start(Args) -> gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%% @doc Starts an ssl listener with default options.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() -> start_link([]).

%% @doc Starts an ssl listener with the given options.
-spec(start_link/1 :: (Args :: start_opts()) -> {'ok', pid()}).
start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% =====
%% init
%% =====

%% @hidden
init(Args) ->
	process_flag(trap_exit, true),
	Port = proplists:get_value(port, Args, 6006),
	PrivDir = pre_server_app:priv_dir(),
	DefaultCertfile = filename:join(PrivDir, "precursors.crt"),
	DefaultKeyfile = filename:join(PrivDir, "key"),
	Certfile = proplists:get_value(certfile, Args, DefaultCertfile),
	Keyfile = proplists:get_value(keyfile, Args, DefaultKeyfile),
	SimpleOpts = [list, {packet, raw}, {reuseaddr, true},
		{certfile, Certfile}, {keyfile, Keyfile}, {keepalive, true},
		{backlog, 30}, {active, false}, binary],
	Poolsize = proplists:get_value(poolsize, Args, 5),
	case ssl:listen(Port, SimpleOpts) of
		{ok, Listen_socket} ->
			Acceptors = spawn_acceptors(Listen_socket, Poolsize),
			%%Create first accepting process
			%Acceptor = spawn_link(?MODULE, spawn_acceptor, [Listen_socket]),
			?info("Started on port ~p", [Port]),
			{ok, #state{poolsize = Poolsize, listener = Listen_socket, acceptors= Acceptors}};
		{error, Reason} ->
			?warning("Could not start ssl:  ~p", [Reason]),
			{stop, Reason}
	end.

%% =====
%% handle_call
%% =====

%% @hidden
handle_call(Req, _From, State) ->
	?debug("Unhandled call:  ~p", [Req]),
	{reply, unknown, State}.

%% =====
%% handle_cast
%% =====

%% @hidden
handle_cast(Req, State) ->
	?debug("Unhandled cast:  ~p", [Req]),
	{noreply, State}.

%% =====
%% handle_info
%% =====

%% @hidden
handle_info({'EXIT', Pid, Cause}, #state{acceptors = Acceptors} = State) ->
	case Cause of
		Normies when Normies =:= normal; Normies =:= shutdown -> ok;
		_ -> ?info("Acceptor ~p died due to ~p", [Pid, Cause])
	end,
	CleanAcceptors = lists:delete(Pid, Acceptors),
	NewAcceptors = spawn_acceptors(State#state.listener, State#state.poolsize, CleanAcceptors),
	{noreply, State#state{acceptors = NewAcceptors}};

handle_info(Req, State) ->
	?debug("Unhandled info:  ~p", [Req]),
	{noreply, State}.

%% =====
%% terminate
%% =====

%% @hidden
terminate(Cause, _State) when Cause =:= normal; Cause =:= shutdown ->
	ok;
terminate(Cause, _State) ->
	?warning("ssl listener died due to ~p", [Cause]),
	ok.

%% =====
%% code_change
%% =====

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% =====
%% Internal 
%% =====

spawn_acceptors(ListSock, Size) ->
	spawn_acceptors(ListSock, Size, []).

spawn_acceptors(_ListSock, Size, Acc) when Size =:= length(Acc) ->
	Acc;
spawn_acceptors(ListSock, Size, Acc) ->
	Pid = spawn_link(?MODULE, spawn_acceptor, [ListSock]),
	spawn_acceptors(ListSock, Size, [Pid | Acc]).

%% @doc This is used internally to start a new process to accept
%% connections.
spawn_acceptor(Socket) ->
	case ssl:transport_accept(Socket) of
		{ok, NewSocket} ->
			case ssl:ssl_accept(NewSocket, 3000) of
				ok ->
					{ok, Pid} = pre_client_manager:start_client(NewSocket),
					ssl:controlling_process(NewSocket, Pid),
					exit(normal);
				{error, Reason} ->
					?notice("Ssl connect did not cmplete:  ~p", [Reason]),
					exit(Reason)
			end;
		{error, Reason} ->
			?notice("ssl transport accept errored:  ~p", [Reason]),
			exit(Reason)
	end.
