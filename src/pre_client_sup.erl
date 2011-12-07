-module(pre_client_sup).

-behavior(supervisor).

-include("log.hrl").

-export([
	start_link/0,
	start_link/1,
	init/1
]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() -> start_link([]).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

% ===== init =====

init(Args) ->
	SslOpts = proplists:get_value(ssl_port_opts, Args, []),
	SslKid = ?CHILD(pre_ssl_listener, worker, SslOpts),

	TcpListener = proplists:get_value(tcp_port_opts, Args, []),
	TcpKid = ?CHILD(pre_tcp_listener, worker, TcpListener),

	%UdpListener = proplists:get_value(upd_port_opts, Args, []),
	%UpdKid = ?CHILD(pre_udp_listener, worker, UdpListener),

	ManagerOpts = proplists:get_value(client_manager_opts, Args, []),
	Manager = ?CHILD(pre_client_manager, worker, [ManagerOpts]),

	Kids = [SslKid, TcpKid, Manager],
	{ok, { {one_for_one, 5, 10}, Kids} }.
