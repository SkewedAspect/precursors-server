-module(pre_gen_entity_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

behavior_test_() ->
	{setup, fun() ->
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
		end}

	] end}.

-endif.
