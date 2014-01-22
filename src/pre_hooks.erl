-module(pre_hooks).
-behaviour(gen_server).

-include("log.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(hook_key, {pid, hook, mod}).
-record(hook, {
	key :: #hook_key{}, func, info, nodes, mon
}).

% public api
-export([create_ets/0, add_hook/3, add_hook/4, add_hook/5, drop_hook/1,
	start_link/0, trigger_hooks/2, trigger_hooks/3, async_trigger_hooks/2,
	async_trigger_hooks/3]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% ===================================================================
%% Public API
%% ===================================================================

start_link() ->
	pre_hooks:create_ets(),
	Result = gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []),
	pre_client_hooks:register_hooks(),
	pre_channel_ping:register_hooks(),
	pre_channel_input:register_hooks(),
	pre_channel_entity:register_hooks(),
	Result.

add_hook(Hook, Mod, Func) ->
	add_hook(Hook, Mod, Func, undefined, global).

add_hook(Hook, Mod, Func, Info) ->
	add_hook(Hook, Mod, Func, Info, global).

add_hook(Hook, Mod, Func, Info, Nodes) ->
	Pid = self(),
	gen_server:cast(?MODULE, {add_hook, Pid, Hook, Mod, Func, Info, Nodes}).

drop_hook(Pid) ->
	gen_server:cast(?MODULE, {drop_hook, Pid}).

trigger_hooks(Hook, Args) ->
	trigger_hooks(Hook, Args, first).

trigger_hooks(Hook, Args, Mode) ->
	?debug("trigger_hooks/3 ~p ~p ~p", [Hook, Args, Mode]),
	Node = node(),
	QH = qlc:q([HookRec ||
		#hook{key = #hook_key{hook = AHook}, nodes = ANodes} = HookRec <- ets:table(?MODULE),
		AHook == Hook, (ANodes == global orelse lists:member(Node, ANodes))]),
	Hooks = qlc:e(QH),
	Acc = case Mode of
		first -> first;
		all -> []
	end,
	run_hooks(Hooks, Args, Acc).

async_trigger_hooks(Hook, Args) ->
	async_trigger_hooks(Hook, Args, first).

async_trigger_hooks(Hook, Args, Mode) ->
	?debug("async_trigger_hooks/3 ~p ~p ~p", [Hook, Args, Mode]),
	spawn(fun() -> trigger_hooks(Hook, Args, Mode) end).

create_ets() ->
	% I'm making the ets public so the table can be created by some other pid, but the hooks server can edit it.
	% Basically, have the application start function call this, then start the hooks server later. This means the ets
	% table won't go away if the hooks server randomly dies, requiring setting up every hook again.
	ets:new(?MODULE, [named_table, public, {keypos, 2}]).

%% ===================================================================
%% gen_server
%% ===================================================================

%% -------------------------------------------------------------------
%% init
%% -------------------------------------------------------------------

init({}) ->
	{ok, state}.

%% -------------------------------------------------------------------
%% handle_call
%% -------------------------------------------------------------------

handle_call(_, _, State) ->
	{reply, {error, invalid}, State}.

%% -------------------------------------------------------------------
%% handle_cast
%% -------------------------------------------------------------------

handle_cast({add_hook, Pid, Hook, Mod, Func, Info, Nodes}, State) ->
	Monref = erlang:monitor(process, Pid),
	Rec = #hook{key = #hook_key{pid = Pid, hook = Hook, mod = Mod}, func = Func, info = Info,
		nodes = Nodes, mon = Monref},
	ets:insert(?MODULE, [Rec]),
	{noreply, State};

handle_cast(_, State) ->
	{noreply, State}.

%% -------------------------------------------------------------------
%% handle_info
%% -------------------------------------------------------------------

handle_info({'DOWN', _Monref, process, Pid, _Reason}, State) ->
	QH = qlc:q([HookKey ||
		#hook{key = #hook_key{pid = APid} = HookKey} <- ets:table(?MODULE),
		APid == Pid]),
	HookKeys = qlc:e(QH),
	lists:map(fun(Key) -> ets:delete(?MODULE, Key) end, HookKeys),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

%% -------------------------------------------------------------------
%% terminate
%% -------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("And so I die:  ~p", [Reason]).

%% -------------------------------------------------------------------
%% code_change
%% -------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

run_hooks([], _Args, first) ->
	{error, not_handled};

run_hooks([HookRec | Tail], Args, Acc) ->
	#hook{key = #hook_key{mod = Mod}, func = Func, info = Info} = HookRec,
	try erlang:apply(Mod, Func, [Info | Args]) of
		{ok, Val} when Acc == first ->
			{ok, Val};
		{ok, Val} ->
			run_hooks(Tail, Args, [Val | Acc]);
		Else ->
			?debug("hook returned bad value:  ~p.  Hook:  ~p", [Else, HookRec]),
			run_hooks(Tail, Args, Acc)
	catch
		What:Why ->
			?notice("Hook errored:  ~p:~p;  Hook:  ~p;  Args:  ~p", [What, Why, HookRec, [Info | Args]]),
			run_hooks(Tail, Args, Acc)
	end;

run_hooks([], _Args, Acc) ->
	{ok, Acc}.
