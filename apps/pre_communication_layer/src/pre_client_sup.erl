%%% @doc pre_client_sup.

-module(pre_client_sup).

-behavior(supervisor).

% API
-export([start_link/0, start_child/1, running_children/0]).

% Supervisor
-export([init/1]).

%% ---------------------------------------------------------------------------------------------------------------------
%% External API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Starts the supervisor for client connections.
-spec start_link() -> {'ok', pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Starts a new client connection process.
-spec start_child(SslProto :: pid()) -> {'ok', pid()}.
start_child(SslProto) ->
	Cookie = make_bin_ref(),

	supervisor:start_child(?MODULE, [SslProto, Cookie]).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Returns a list of currently running children pids.
-spec running_children() -> [pid()].
running_children() ->
	Kids = supervisor:which_children(?MODULE),
	[Child || {_Id, Child, _Type, _Module} <- Kids, is_pid(Child)].

%% ---------------------------------------------------------------------------------------------------------------------
%% Supervisor behavior
%% ---------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
	% Build an ets table to handle the TCP Cookie lookup.
	pre_client:ensure_ets(),

	ChildSpec = {undefined, {pre_client, start_link, []}, temporary, 5, worker, dynamic},
	{ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.

% Make a ref and then wrap it in a bin
make_bin_ref() ->
	list_to_binary(io_lib:format("~p", [make_ref()])).

