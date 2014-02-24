%%% @doc pre_client_sup} 
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_client_sup).

-behavior(supervisor).

% API
-export([start_link/2, start_child/1, running_children/0]).

% Supervisor
-export([init/1]).

%% ---------------------------------------------------------------------------------------------------------------------
%% External API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Starts a new client connection process. Takes the SSL proto pid, and the tcp cookie.
-spec start_link(SslProto :: pid(), Cookie :: binary()) -> {'ok', pid()}.
start_link(SslProto, Cookie) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Starts a child.
-spec start_child(Args :: list()) -> {'ok', pid()}.
start_child(Args) ->
	supervisor:start_child(?MODULE, Args).

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
	_Ets = ets:new(client_ets, [named_table, public, set, {keypos, 1}]),


	ChildSpec = {undefined, {pre_client, start_link, []}, transient, 5, worker, dynamic},
	{ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.
% Code Here.