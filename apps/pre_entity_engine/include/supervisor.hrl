%% --------------------------------------------------------------------------------------------------------------------
%% Restart Strategies

% For "dynamic" supervisors (where no children are started initially, and every new child is running the same code),
% use simple_one_for_one, and give up and die if a child process is restarted more than 5 times in 10 seconds.
-define(RESTART_STRATEGY_DYNAMIC, {simple_one_for_one, 5, 10}).

% For "static" supervisors (where a collection of children are started initially, and each may run different code),
% default to one_for_one, and give up and die if a child process is restarted more than 5 times in 10 seconds.
-define(RESTART_STRATEGY_STATIC, {one_for_one, 5, 10}).

%% --------------------------------------------------------------------------------------------------------------------
%% Shutdown Strategies

% - For 'supervisor' child processes: "If the child process is another supervisor, Shutdown should be set to infinity
%     to give the subtree ample time to shutdown." (from supervisor(3))
% - For 'gen_event' child processes: "When an event manager is stopped, it will give each of the installed event
%     handlers the chance to clean up by calling terminate/2, the same way as when deleting a handler." (from
%     http://www.erlang.org/doc/design_principles/events.html#id71067) So, if you don't use a timeout or infinity
%     shutdown strategy, it will also be effectively brutal_kill'ing all event handlers.
% - For 'gen_server' and 'gen_fsm' child processes: "If it is necessary to clean up before termination, the shutdown
%     strategy must be a timeout value and [the module] must be set to trap exit signals in the init function." (from
%     http://www.erlang.org/doc/design_principles/gen_server_concepts.html#id66148 and
%     http://www.erlang.org/doc/design_principles/fsm.html#id69218)
%
% So, basically:
% - if the child is a supervisor, use infinity
% - if the child is a gen_event or the child's init/1 includes `process_flag(trap_exit, true)', use a timeout or
%     infinity (default to a 5 second timeout between calling `exit(Child, shutdown)' and calling `exit(Child, kill)')
% - otherwise, we should probably use brutal_kill, since a timeout gains us nothing
-define(DEF_SHUTDOWN_SUPERVISOR, infinity).
-define(DEF_SHUTDOWN_CLEANUP, 5000).
-define(DEF_SHUTDOWN_NO_CLEANUP, brutal_kill).

%% --------------------------------------------------------------------------------------------------------------------
%% Child Specifications

-define(CHILD_SUPERVISOR(Module, Args, Lifetime, Shutdown),
	{Module, {Module, start_link, Args}, Lifetime, Shutdown, supervisor, [Module]}
).
-define(CHILD_SUPERVISOR(Module, Args, Lifetime), ?CHILD_SUPERVISOR(Module, Args, Lifetime, ?DEF_SHUTDOWN_SUPERVISOR)).
-define(CHILD_SUPERVISOR(Module, Args), ?CHILD_SUPERVISOR(Module, Args, permanent)).

%% --------------------------------------------------------------------------------------------------------------------

-define(CHILD_GEN_SERVER(Module, Args, Lifetime, Shutdown),
	{Module, {Module, start_link, Args}, Lifetime, Shutdown, worker, [Module]}
).
-define(CHILD_GEN_SERVER(Module, Args, Lifetime), ?CHILD_GEN_SERVER(Module, Args, Lifetime, ?DEF_SHUTDOWN_NO_CLEANUP)).
-define(CHILD_GEN_SERVER(Module, Args), ?CHILD_GEN_SERVER(Module, Args, permanent)).

%% --------------------------------------------------------------------------------------------------------------------

-define(CHILD_GEN_EVENT(Module, Args, Lifetime, Shutdown),
	{Module, {Module, start_link, Args}, Lifetime, Shutdown, worker, dynamic}
).
-define(CHILD_GEN_EVENT(Module, Args, Lifetime), ?CHILD_GEN_EVENT(Module, Args, Lifetime, ?DEF_SHUTDOWN_CLEANUP)).
-define(CHILD_GEN_EVENT(Module, Args), ?CHILD_GEN_EVENT(Module, Args, permanent)).

% Name: {local, Name} | {global, GlobalName} | {via, Module, ViaName}
-define(CHILD_GEN_EVENT_SIMPLE(Name, Args, Lifetime, Shutdown),
	{Name, {gen_event, start_link, [{local, Name}]}, Lifetime, Shutdown, worker, dynamic}
).
-define(CHILD_GEN_EVENT_SIMPLE(Module, Args, Lifetime),
	?CHILD_GEN_EVENT_SIMPLE(Module, Args, Lifetime, ?DEF_SHUTDOWN_CLEANUP)
).
-define(CHILD_GEN_EVENT_SIMPLE(Module, Args), ?CHILD_GEN_EVENT_SIMPLE(Module, Args, permanent)).

%% --------------------------------------------------------------------------------------------------------------------
%% Supervisor Definitions

-define(DYNAMIC_SUPERVISOR_DEF(ChildSpec), {ok, {?RESTART_STRATEGY_DYNAMIC, [ChildSpec]}}).

-define(STATIC_SUPERVISOR_DEF(ChildSpecs), {ok, {?RESTART_STRATEGY_STATIC, ChildSpecs}}).

%% --------------------------------------------------------------------------------------------------------------------
%% `init/1' Implementations

-define(DYNAMIC_SUPERVISOR_INIT(ChildSpec),
	init(supervisor) ->
		?DYNAMIC_SUPERVISOR_DEF(ChildSpec)
).

-define(STATIC_SUPERVISOR_INIT(ChildSpecs),
	init(supervisor) ->
		?STATIC_SUPERVISOR_DEF(ChildSpecs)
).

% Pseudo-spec to help me implement this...
%-spec init(supervisor) -> SupervisorInitResult when
%	SupervisorInitResult :: {RestartStrategy, [ChildSpec]},
%		RestartStrategy :: {Strategy, MaxR, MaxT},
%			Strategy :: 'one_for_all' | 'one_for_one' | 'rest_for_one' | 'simple_one_for_one',
%			MaxR :: integer() >= 0,
%			MaxT :: integer() > 0,
%			% Give up if more than MaxR restarts occur in MaxT seconds.
%		ChildSpec :: {ChildID, StartFunc, Restart, Shutdown, Type, Modules},
%			ChildID :: term(),  % Not a pid().
%			StartFunc :: {M, F, A},
%				M :: module(),
%				F :: atom(),
%				A :: [term()],
%			Restart :: 'permanent' | 'transient' | 'temporary',
%			Shutdown :: 'brutal_kill' | integer() > 0 | 'infinity',
%			Type :: 'worker' | 'supervisor',
%			Modules :: modules().
