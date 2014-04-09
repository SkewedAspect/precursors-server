-module(pre_entity_sim_timer_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
	{ok, Sup} = pre_ge_sup:start_link(1),
	Got = pre_entity_sim_timer:start_link(),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	preetu:kill(Pid),
	preetu:kill(pre_ge_sup).

functionality_test_() ->
	{setup, fun() ->
		pre_ge_sup:start_link(1),
		pre_entity_sim_timer:start_link(),
		meck:new(callback, [non_strict])
	end,
	fun(_) ->
		meck:unload(callback),
		preetu:kill(pre_entity_sim_timer),
		preetu:kill(pre_ge_sup)
	end,
	fun(_) -> [

		{"timer fires, sims are runned", fun() ->
			meck:expect(callback, init, fun(_) ->
				{ok, state}
			end),
			Self = self(),
			meck:expect(callback, simulate, fun(_, state) ->
				Self ! continue,
				{ok, state}
			end),
			[Ge] = pre_ge_sup:running_children(),
			pre_gen_entity:add_entity(Ge, 10012, callback, {}),
			receive continue -> ok end
		end}

	] end}.

-endif.
