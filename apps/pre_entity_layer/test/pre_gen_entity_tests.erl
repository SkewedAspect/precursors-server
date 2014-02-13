-module(pre_gen_entity_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

behavior_test_() ->
	{setup, fun() ->
		mnesia:start(),
		lager:start(),
		{ok, GE} = gen_event:start(),
		meck:new(callback, [non_strict]),
		GE
	end,
	fun(GE) ->
		gen_event:stop(GE)
	end,
	fun(GE) -> [

		{"able to create mnesia table", fun() ->
			Got = pre_gen_entity:ensure_mnesia_table(),
			?assertEqual(ok, Got),
			?assertEqual(true, mnesia:table_info(pre_gen_entity, local_content)),
			?assertEqual([key, val], mnesia:table_info(pre_gen_entity, attributes))
		end},

		{"init called", fun() ->
			meck:expect(callback, init, fun([1, 2]) ->
				{ok, state}
			end),
			pre_gen_entity:add_entity(GE, 1, callback, [1,2]),
			?assert(meck:called(callback, init, [[1,2]], '_'))
		end},

		{"after init, state exists in mnesia table", fun() ->
			Gots = mnesia:dirty_read(pre_gen_entity, {callback, 1}),
			?assertEqual(1, length(Gots)),
			Got = hd(Gots),
			?assertEqual({pre_gen_entity, {callback, 1}, state}, Got)
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
		end},

		{"after state update, persistent version persists", fun() ->
			Gots = mnesia:dirty_read(pre_gen_entity, {callback, 1}),
			?assertEqual(1, length(Gots)),
			Got = hd(Gots),
			?assertEqual({pre_gen_entity, {callback, 1}, newstate}, Got)
		end}

	] end}.

-endif.
