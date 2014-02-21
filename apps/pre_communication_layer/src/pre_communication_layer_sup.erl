%%% @doc Supervise the processes needed to allow clients to connect and log in.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_communication_layer_sup).

-behaviour(supervisor).

% API
-export([
	start_link/0,
	start_link/1
]).

% Supervisor callbacks
-export([init/1]).

% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts up using default arguments.

-spec start_link() -> {'ok', pid()}.

start_link() -> start_link([]).


%% @doc Starts up with the given arguements.

-spec start_link(Args :: StartOptions) -> {'ok', pid()} when
	SslOptions :: [any()],
	TcpOptions :: [any()],
	ClientManagerOpts :: [any()],
	StartOption :: SslOptions | TcpOptions | ClientManagerOpts,
	StartOptions :: [StartOption].

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% --------------------------------------------------------------------------------------------------------------------
%% Supervisor callbacks
%% --------------------------------------------------------------------------------------------------------------------

%% @hidden
init(Args) ->
	%SslOpts = proplists:get_value(ssl_port_opts, Args, []),
	%SslKid = ?CHILD(pre_ssl_listener, worker, SslOpts),

	%TcpListener = proplists:get_value(tcp_port_opts, Args, []),
	%TcpKid = ?CHILD(pre_tcp_listener, worker, TcpListener),

  lager:error("HELP ME!"),

  % Start Ranch SSL listener
  SslAcceptors = proplists:get_value(ssl_acceptors, Args, 100),
  SslOpts = proplists:get_value(ssl_port_opts, Args, [{port, 6006}]),
  {ok, _} = ranch:start_listener(ssl, SslAcceptors, ranch_ssl, SslOpts, pre_ssl_proto, []),

  % Start Ranch TCP listener
  TcpAcceptors = proplists:get_value(tcp_acceptors, Args, 100),
  TcpOpts = proplists:get_value(tcp_port_opts, Args, [{port, 6007}]),
  {ok, _} = ranch:start_listener(tcp, TcpAcceptors, ranch_tcp, TcpOpts, pre_tcp_proto, []),

	ManagerOpts = proplists:get_value(client_manager_opts, Args, []),
	Manager = ?CHILD(pre_client_manager, worker, [ManagerOpts]),

  %Kids = [SslKid, TcpKid, Manager],
	Kids = [Manager],
	{ok, { {one_for_one, 5, 10}, Kids} }.
