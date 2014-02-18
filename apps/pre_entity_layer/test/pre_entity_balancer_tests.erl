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
		{ok, Sup} = pre_ge_sup:start_link(5),
		{ok, B} = pre_entity_balancer:start_link([]),
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

gen_event_exit_test_() ->
	{setup, fun() ->
		{ok, Sup} = pre_ge_sup:start_link(1),
		{ok, B} = pre_entity_balancer:start_link([]),
		meck:new(callback, [non_strict]),
		{Sup, B}
	end,
	fun({Sup, B}) ->
		preetu:kill(B),
		preetu:kill(Sup),
		meck:unload(callback)
	end,
	fun({_Sup, _B}) -> [

		{"a new gen_event is started when old one killed", fun() ->
			Self = self(),
			meck:expect(callback, init, fun(_) ->
				Self ! continue,
				{ok, state}
			end),
			Twenty = lists:seq(1, 20),
			lists:foreach(fun(N) ->
				pre_entity_balancer:add_entity(callback, N + 200, undefined)
			end, Twenty),
			lists:foreach(fun(_) ->
				receive continue -> ok end
			end, Twenty),
			[{GE, _}] = pre_entity_balancer:stats(),
			?debugFmt("Old GE: ~p", [GE]),
			preetu:kill(GE),
			timer:sleep(1000),
			GotStats = pre_entity_balancer:stats(),
			?assertMatch([{_, 0}], GotStats),
			[{NewGE, _}] = GotStats,
			?debugFmt("new GE: ~p", [NewGE]),
			?assertNotEqual(GE, NewGE)
		end}

	] end}.




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
