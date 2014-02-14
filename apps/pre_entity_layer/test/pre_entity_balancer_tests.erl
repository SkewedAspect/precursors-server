-module(pre_entity_balancer_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
	{ok, Sup} = pre_ge_sup:start_link(),
	Got = pre_entity_balancer:start_link(),
	?assertMatch({ok, _Pid}, Got),
	{ok, Pid} = Got,
	preetu:kill(Pid),
	preetu:kill(Sup).

functionality_test_() ->
	{setup, fun() ->
		{ok, Sup} = pre_ge_sup:start_link(),
		{ok, B} = pre_entity_balancer:start_link([
			{workers, 5}
		]),
		meck:new(callback, [non_strict]),
		{Sup, B}
	end,
	fun({Sup, B}) ->
		meck:unload(callback),
		[preetu:kill(P) || P <- [B, Sup]]
	end,
	fun(_) -> [

		{"has correct number of running workers", fun() ->
			Got = pre_ge_sup:running_children(),
			?assertEqual(5, length(Got))
		end},

		{"check current load", fun() ->
			Got = pre_entity_balancer:stats(),
			?assertEqual(5, length(Got)),
			lists:foreach(fun({Pid, Load}) ->
				?assert(is_pid(Pid)),
				?assertEqual(0, Load)
			end, Got)
		end},

		{"adding an entity updates stats", fun() ->
			meck:expect(callback, init, fun([1,2]) ->
				{ok, state}
			end),
			ok = pre_entity_balancer:add_entity(callback, 78, [1,2]),
			Stats = pre_entity_balancer:stats(),
			?assert(lists:all(fun({_, N}) -> N >= 0 end, Stats)),
			?assertEqual(1, lists:foldl(fun({_, N}, Acc) -> Acc + N end, 0, Stats))
		end},

		{"able to notify entities en masse", fun() ->
			Self = self(),
			meck:expect(callback, handle_event, fun(some_event, from, to, data, state) ->
				Self ! continue,
				{ok, state}
			end),
			pre_entity_balancer:notify(some_event, from, to, data),
			Got = receive
				continue -> true
			after 100 -> ?debugMsg("didn't get continue"), false
			end,
			?assert(Got)
		end},

		{"removed entities eventually update stats", fun() ->
			Self = self(),
			meck:expect(callback, handle_event, fun(some_event, from, to, data, _) ->
				remove_entity
			end),
			meck:expect(callback, removed, fun(remove_entity, _) ->
				Self ! continue,
				ok
			end),
			pre_entity_balancer:notify(some_event, from, to, data),
			Got = receive
				continue -> true
			after 100 -> ?debugMsg("didn't get continue"), false
			end,
			% timer until there's something better to watch/check
			timer:sleep(100),
			Stats = pre_entity_balancer:stats(),
			lists:foreach(fun({_Pid, Load}) ->
				?assertEqual(0, Load)
			end, Stats)
		end},

		{"load is balanced", fun() ->
			Self = self(),
			meck:expect(callback, init, fun(_) ->
				Self ! continue,
				{ok, state}
			end),
			lists:foreach(fun(N) ->
				pre_entity_balancer:add_entity(callback, N + 100, []),
				receive continue -> ok end
			end, lists:seq(1, 5)),
			Stats = pre_entity_balancer:stats(),
			lists:foreach(fun({_Pid, N}) ->
				?assertEqual(1, N)
			end, Stats)
		end}

	] end}.

%"add_entity alters stats"
%"entity_removal alters stats"
%"unexpected gen_event death gracefully handled"






funcationality_test_d() ->
	{setup, fun() ->
		_ = pre_entity_balancer:start_link(),
		meck:new(callback, [non_strict])
	end,
	fun(_) ->
		meck:unload(callback),
		pre_entity_balancer:stop()
	end,
	fun(_) -> [

		{"able to add an entity", fun() ->
			Self = self(),
			meck:expect(callback, init, fun([1,2]) ->
				Self ! continue,
				{ok, state}
			end),
			Got = pre_entity_balancer:add_entity(1, callback, [1,2]),
			?assertEqual(ok, Got),
			receive
				continue -> ok
			after 100 ->
				timeout
			end,
			?assert(meck:called(callback, init, [[1,2]], '_'))
		end}

	] end}.

-endif.
