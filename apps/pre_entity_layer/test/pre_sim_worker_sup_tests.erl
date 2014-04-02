-module(pre_sim_worker_sup_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
	Got = pre_sim_worker_sup:start_link(7),
	?assertMatch({ok, _Pid}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	preetu:kill(Pid).

-endif.
