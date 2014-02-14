-module(pre_gen_entity_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

behavior_test_() ->
	{setup, fun() ->
		mnesia:start(),
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
			?assert(meck:called(callback, removed, [remove_entity, newstate], '_')),
			Gots = mnesia:dirty_read(pre_gen_entity, {callback, 1}),
			?assertEqual([], Gots)
		end},

		{"attempt to recover non-existant entity", fun() ->
			Got = pre_gen_entity:recover_entity(GE, 1, callback),
			?assertEqual({error, not_found}, Got)
		end},

		{"recover an entity that does exist", fun() ->
			Rec = {pre_gen_entity, {callback, 3}, a_state},
			mnesia:dirty_write(pre_gen_entity, Rec),
			Got = pre_gen_entity:recover_entity(GE, 3, callback),
			?assertEqual(ok, Got),
			meck:expect(callback, handle_event, fun(some_event, undefined, undefined, data, a_state) ->
				remove_entity
			end),
			Self = self(),
			meck:expect(callback, removed, fun(_, _) ->
				Self ! continue
			end),
			pre_gen_entity:notify(GE, some_event, undefined, undefined, data),
			receive continue -> ok after 100 -> ?debugMsg("didn't get continue") end,
			?assert(meck:called(callback, handle_event, [some_event, undefined, undefined, data, a_state], '_')),
			Gots = mnesia:dirty_read(pre_gen_entity, {callback, 3}),
			?assertEqual([], Gots)
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

		{"recover an entity that is supervised", fun() ->
			Rec = {pre_gen_entity, {callback, 87}, b_state},
			mnesia:dirty_write(pre_gen_entity, Rec),
			meck:expect(callback, handle_event, fun(_, _, _, _, _) ->
				remove_entity
			end),
			meck:expect(callback, removed, fun(_, _) ->
				ok
			end),
			_ = pre_gen_entity:recover_sup_entity(GE, 87, callback),
			pre_gen_entity:notify(GE, some_event, 7, 7, data),
			MsgReceived = receive
				{pre_gen_entity, entity_removed, GE, {callback, 87}} ->
					true
			after 100 ->
				false
			end,
			?assert(MsgReceived)
		end}

	] end}.

gen_event_stop_test_() ->
	{foreach, fun() ->
		_ = pre_gen_entity:ensure_mnesia_table(),
		{ok, Pid} = gen_event:start(),
		meck:new(callback, [non_strict]),
		meck:expect(callback, init, fun(_) ->
			{ok, state}
		end),
		pre_gen_entity:add_entity(Pid, 23, callback, [1,2,3]),
		Pid
	end,
	fun(Pid) ->
		case is_process_alive(Pid) of
			true ->
				gen_event:stop(Pid);
			false ->
				ok
		end,
		meck:unload(callback)
	end,
	[

		fun(GE) -> {"callback can request no persistence", fun() ->
			meck:expect(callback, stopping, fun(state) ->
				ok
			end),
			ok = gen_event:stop(GE),
			?assert(meck:called(callback, stopping, [state], '_')),
			Got = mnesia:dirty_read(pre_gen_entity, {callback, 23}),
			?assertEqual([], Got)
		end} end,

		fun(GE) -> {"callback can request persistence", fun() ->
			meck:expect(callback, stopping, fun(state) ->
				persist
			end),
			ok = gen_event:stop(GE),
			?assert(meck:called(callback, stopping, [state], '_')),
			Got = mnesia:dirty_read(pre_gen_entity, {callback, 23}),
			?assertEqual([{pre_gen_entity, {callback, 23}, state}], Got)
		end} end,

		fun(GE) -> {"callback can request peristence of different state", fun() ->
			meck:expect(callback, stopping, fun(state) ->
				{persist, new_state}
			end),
			ok = gen_event:stop(GE),
			?assert(meck:called(callback, stopping, [state], '_')),
			Got = mnesia:dirty_read(pre_gen_entity, {callback, 23}),
			?assertEqual([{pre_gen_entity, {callback, 23}, new_state}], Got)
		end} end

	]}.

-endif.
