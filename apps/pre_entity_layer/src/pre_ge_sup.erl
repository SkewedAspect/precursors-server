-module(pre_ge_sup).

-behavior(supervisor).

% api
-export([start_link/0, start_link/1, start_child/0, running_children/0]).
% supervisor
-export([init/1]).

start_link() ->
	start_link(5).

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

start_child() ->
	supervisor:start_child(?MODULE, []).

running_children() ->
	Kids = supervisor:which_children(?MODULE),
	[Child || {_Id, Child, _Type, _Module} <- Kids, is_pid(Child)].

init(_) ->
	ChildSpec = {undefined, {gen_event, start_link, []}, transient, 5, worker, dynamic},
	{ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.

