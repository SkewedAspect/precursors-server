-module(pre_ge_sup_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
	Got = pre_ge_sup:start_link(),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	exit(Pid, normal),
	preetu:wait_for_exit(Pid).

use_test_() ->
	{setup, fun() ->
		{ok, Pid} = pre_ge_sup:start_link(),
		Pid
	end,
	fun(Pid) ->
		exit(Pid, normal),
		preetu:wait_for_exit(Pid)
	end,
	fun(_Pid) -> [

		{"able to add a child", fun() ->
			Got = pre_ge_sup:start_child(),
			?assertMatch({ok, _}, Got),
			{ok, P} = Got,
			?assert(is_pid(P))
		end},

		{"able to query about started children", fun() ->
			Got = pre_ge_sup:running_children(),
			?assertMatch([_APid], Got),
			[APid] = Got,
			?assert(is_pid(APid))
		end},

		{"normal exit of child does not get restarted", fun() ->
			[APid] = pre_ge_sup:running_children(),
			gen_event:stop(APid),
			preetu:wait_for_exit(APid),
			Got = pre_ge_sup:running_children(),
			?assertEqual([], Got)
		end}

	] end}.

-endif.
