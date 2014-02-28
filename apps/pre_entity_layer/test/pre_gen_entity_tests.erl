-module(pre_gen_entity_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

behavior_test_() ->
	{setup, fun() ->
		%lager:start(),
		{ok, GE} = gen_event:start(),
		meck:new(callback, [non_strict]),
		GE
	end,
	fun(GE) ->
		meck:unload(callback),
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
		end},

		{"removal of the entity by the callback", fun() ->
			Self = self(),
			meck:expect(callback, handle_event, fun(some_event, some_id, undefined, data, newstate) ->
				remove_entity
			end),
			meck:expect(callback, removed, fun(remove_entity, newstate) ->
				Self ! continue,
				ok
			end),
			pre_gen_entity:notify(GE, some_event, some_id, undefined, data),
			receive continue -> ok after 100 -> ?debugMsg("didn't get continue") end,
			?assert(meck:called(callback, handle_event, [some_event, some_id, undefined, data, newstate], '_')),
			?assert(meck:called(callback, removed, [remove_entity, newstate], '_'))
		end},

		{"add en enitty that is supervised", fun() ->
			meck:expect(callback, init, fun([3,4]) ->
				{ok, state}
			end),
			meck:expect(callback, handle_event, fun(_, _, _, _, _) ->
				remove_entity
			end),
			meck:expect(callback, removed, fun(_, _) ->
				ok
			end),
			_ = pre_gen_entity:add_sup_entity(GE, 7, callback, [3, 4]),
			pre_gen_entity:notify(GE, some_event, 7, 7, data),
			MsgReceived = receive
				{pre_gen_entity, entity_removed, GE, {callback, 7}} ->
					true
			after 100 ->
				false
			end,
			?assert(MsgReceived)
		end},

		{"able to send an event after a time", fun() ->
			_Timer = pre_gen_entity:notify_after(100, timer, undefined, undefined, data),
			Got = receive Msg -> Msg after 150 -> timeout end,
			?assertNotEqual(timeout, Got)
		end},

		{"able to cancel a notify_after", fun() ->
			Timer = pre_gen_entity:notify_after(100, timer, undefined, undefined, data),
			pre_gen_entity:cancel_notify(Timer),
			Got = receive Msg -> Msg after 150 -> timeout end,
			?assertEqual(timeout, Got)
		end},

		{"notify after hits callback", fun() ->
			Self = self(),
			meck:expect(callback, init, fun(_) ->
				_ = pre_gen_entity:notify_after(100, timer, undefined, undefined, data),
				{ok, state}
			end),
			meck:expect(callback, handle_event, fun(timer, undefined, undefined, data, state) ->
				Self ! continue,
				{ok, state}
			end),
			pre_gen_entity:add_entity(GE, 65, callback, []),
			receive continue -> ok end
		end}

	] end}.

-endif.
