%% @doc Supervises the entitiy event system, and ensures the pre_ge_sup
%% and pre_entity_balancer are running in the proper dependency order.
-module(pre_entity_event_sup).

-behavior(supervisor).

% API
-export([start_link/1]).
% supervisor
-export([init/1]).

%% ---------------------------------------------------------------------
%% api
%% ---------------------------------------------------------------------

%% @doc Start named and linked to the calling process.
-spec start_link(N :: pos_integer()) -> {'ok', pid()}.

start_link(N) when is_integer(N), N > 0 ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, N).

%% ---------------------------------------------------------------------
%% supervisor
%% ---------------------------------------------------------------------

%% @private
init(N) ->
	PreGESup = {pre_ge_sup, {pre_ge_sup, start_link, [N]}, permanent, 5, supervisor, [?MODULE]},
	Balancer = {pre_entity_balancer, {pre_entity_balancer, start_link, [[]]}, permanent, 5, worker, [?MODULE]},
	SimTimer = {pre_entity_sim_timer, {pre_entity_sim_timer, start_link, []}, permanent, 5, worker, [?MODULE]},
	{ok, {{one_for_one, 5, 10}, [PreGESup, Balancer, SimTimer]}}.

