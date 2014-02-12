-module(pre_gen_entity_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

behavior_test_() ->
	{setup, fun() ->
		lager:start(),
		{ok, GE} = gen_event:start(),
		meck:new(callback, [non_strict]),
		GE
	end,
	fun(GE) ->
		gen_event:stop(GE)
	end,
	fun(GE) -> [

		{"init called", fun() ->
			meck:expect(callback, init, fun([1, 2]) ->
				{ok, state}
			end),
			pre_gen_entity:add_entity(GE, 1, callback, [1,2]),
			?assert(meck:called(callback, init, [[1,2]], '_'))
		end},

		{"handle_event, simple", fun() ->
			Self = self(),
			meck:expect(callback, handle_event, fun(some_event, undefined, undefined, data, state) ->
				Self ! continue,
				{ok, newstate}
			end),
			pre_gen_entity:notify(GE, some_event, undefined, undefined, data),
			receive
				continue ->
					ok
			after 100 ->
				?debugMsg("Didn't get continue soon enough")
			end,
			?assert(meck:called(callback, handle_event, [some_event, undefined, undefined, data, state], '_'))
		end}

	] end}.

-endif.
