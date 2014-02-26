%%% @doc The entity simulation worker supervisor.
%%%
%%% This module starts and supervisors the simulation worker processes. (see `pre_sim_worker')
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_sim_worker_sup).

-include("simulation.hrl").

% API
-export([start_link/0, start_link/1]).

% supervisor callbacks
-export([init/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Same as `start_link(5).'
%% @see start_link/1

-spec start_link() -> {'ok', pid()}.

start_link() ->
	start_link(5).

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Start the named supervisor `pre_sim_worker_sup' linked to the calling process. `N' is the number of worker
%% processes to run. This is a `simple_one_for_one' supervisor with a restart strategy of transient. This means that
%% all of the children are basically the same, and if they exit with reason `normal' or `shutdown' they are not
%% restarted.

-spec start_link(N :: non_neg_integer()) -> {'ok', pid()}.

start_link(N) ->
	case supervisor:start_link({local, ?MODULE}, ?MODULE, undefined) of
		{ok, _} = Out ->
			lists:foreach(
				fun(_) ->
					start_child()
				end,
				lists:seq(1, N)
			),
			Out;
		Else ->
			Else
	end.

%% ---------------------------------------------------------------------------------------------------------------------
%% supervisor
%% ---------------------------------------------------------------------------------------------------------------------

%% @private
init(_) ->
	lager:info("pre_sim_worker_sup:init()", []),

	pre_sim_worker:init_node(),

	% Start the simulation timer
	case timer:apply_interval(?INTERVAL, pre_sim_worker, simulate, []) of
		{ok, _TRef} ->
			ok;
		{error, Reason} ->
			lager:error("Error starting simulation timer: ~p", [Reason])
	end,

	ChildSpec = {undefined, {pre_sim_worker, start_link, []}, transient, 5, worker, dynamic},
	{ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.

%% ---------------------------------------------------------------------------------------------------------------------
%% Internal API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Starts a child.
%% @hidden

-spec start_child() -> {'ok', pid()}.

start_child() ->
	supervisor:start_child(?MODULE, []).
