%% @doc Supervisor the processes needed to get clients connected and
%% logged in.
-module(pre_client_sup).

-behaviour(supervisor).

-export([
	start_link/0,
	start_link/1,
	init/1
]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% @doc Starts up using default arguments.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() -> start_link([]).

-type(ssl_options() :: [any()]).
-type(tcp_options() :: [any()]).
-type(client_manager_opts() :: [any()]).
-type(start_option() :: ssl_options() | tcp_options() |
	client_manager_opts()).
-type(start_options() :: [start_option()]).
%% @doc Starts up with the given arguements.
-spec(start_link/1 :: (Args :: start_options()) -> {'ok', pid()}).
start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

% ===== init =====

%% @hidden
init(Args) ->
	SslOpts = proplists:get_value(ssl_port_opts, Args, []),
	SslKid = ?CHILD(pre_ssl_listener, worker, SslOpts),

	TcpListener = proplists:get_value(tcp_port_opts, Args, []),
	TcpKid = ?CHILD(pre_tcp_listener, worker, TcpListener),

	%UdpListener = proplists:get_value(udp_port_opts, Args, []),
	%UdpKid = ?CHILD(pre_udp_listener, worker, UdpListener),

	ManagerOpts = proplists:get_value(client_manager_opts, Args, []),
	Manager = ?CHILD(pre_client_manager, worker, [ManagerOpts]),

	Kids = [SslKid, TcpKid, Manager],
	{ok, { {one_for_one, 5, 10}, Kids} }.