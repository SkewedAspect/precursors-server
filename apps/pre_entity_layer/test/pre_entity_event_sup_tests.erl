-module(pre_entity_event_sup_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
	Got = pre_entity_event_sup:start_link(7),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	preetu:kill(Pid).

-endif.
