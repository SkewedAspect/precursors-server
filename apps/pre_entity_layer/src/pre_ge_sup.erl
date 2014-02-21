%%% @doc Supervises and starts the pre_gen_entity gen_servers.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_ge_sup).

-behavior(supervisor).

% API
-export([start_link/0, start_link/1, start_child/0, running_children/0]).

% Supervisor
-export([init/1]).

%% ---------------------------------------------------------------------------------------------------------------------
%% External API
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc By default, we start 5 children.
-spec start_link() -> {ok, any()} | any().

start_link() ->
  start_link(5).

%% @doc Starts N children.
-spec start_link(N :: integer()) -> {ok, any()} | any().

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

%% @doc Starts the child.
-spec start_child() -> any().

start_child() ->
	supervisor:start_child(?MODULE, []).

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Returns a list of currently running children.
-spec running_children() -> list().

running_children() ->
	Kids = supervisor:which_children(?MODULE),
	[Child || {_Id, Child, _Type, _Module} <- Kids, is_pid(Child)].

%% ---------------------------------------------------------------------------------------------------------------------
%% Supervisor behavior
%% ---------------------------------------------------------------------------------------------------------------------

init(_) ->
	ChildSpec = {undefined, {gen_event, start_link, []}, transient, 5, worker, dynamic},
	{ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.

