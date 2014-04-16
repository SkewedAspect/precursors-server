%%% @doc Supervisor for gen_event managers. This is a named supervisor,
%%% so only one can be running at a time.

-module(pre_ge_sup).

-behavior(supervisor).

% API
-export([start_link/0, start_link/1, start_child/0, running_children/0]).

% Supervisor
-export([init/1]).

%% ---------------------------------------------------------------------------------------------------------------------
%% External API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Same as `start_link(5).'
%% @see start_link/1
-spec start_link() -> {'ok', pid()}.
start_link() ->
  start_link(5).

%% @doc Start the named supervisor `pre_ge_sup' linked to the calling
%% process. `N' is the number of gen_event managers to run. This is a
%% `simple_one_for_one' supervisor with a restart strategy of transient.
%% This means that all of the children are basically the same, and if they
%% exit with reason `normal' or `shutdown' they are not restarted.
-spec start_link(N :: non_neg_integer()) -> {'ok', pid()}.
start_link(N) ->
  case supervisor:start_link({local, ?MODULE}, ?MODULE, undefined) of
    {ok, _} = Out ->
      lists:foreach(fun(_) ->
        start_child()
      end, lists:seq(1, N)),
      Out;
    Else ->
      Else
  end.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Starts a child.
-spec start_child() -> {'ok', pid()}.
start_child() ->
	supervisor:start_child(?MODULE, []).

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
	ChildSpec = {undefined, {gen_event, start_link, []}, transient, 5, worker, dynamic},
	{ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.

