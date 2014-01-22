-module(pre_gen_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").
-include("log.hrl").
%-include("internal_auth.hrl").

acting_as_manager_test_() ->
	{setup,
		fun() ->
			meck:new(mnesia),
			{ok, Pid} = pre_gen_auth:start_link([])
		end,
		fun(_) ->
			meck:unload(mnesia)
		end,
		fun(_) ->
			[
				{"User not found", fun() ->
					meck:expect(mnesia, transaction, fun(_) -> {atomic, []} end),

					Out = pre_gen_auth:authenticate("gerald", "herbert"),
					?assertEqual({deny, "No backends definitive"}, Out),
					?assert(meck:validate(mnesia))
				end},

				{"User found and denied", fun() ->
					ok = meck:new(goober_back, [non_strict]),
					meck:expect(goober_back, init,
						fun(substate) ->
							{ok, substate}
						end),
					meck:expect(goober_back, handle_authentication,
						fun("gerald", "herber", substate) ->
							{deny, "I'm in a bad mood"}
						end),
					pre_gen_auth:add_backend(goober_back, 1, substate),

					Out = pre_gen_auth:authenticate("gerald", "herber"),
					?assertEqual({deny, "I'm in a bad mood"}, Out),
					?assert(meck:validate(goober_back)),

					meck:unload(goober_back),
					pre_gen_auth:remove_backend(goober_back, 1)
				end},

				{"User allowed access", fun() ->
					ok = meck:new(superboy_prime, [non_strict]),
					meck:expect(superboy_prime, init,
						fun(substate) ->
							{ok, substate}
						end),
					meck:expect(superboy_prime, handle_authentication,
						fun("gerald", "herber", substate) ->
							{allow, "gerald"}
						end),
					pre_gen_auth:add_backend(superboy_prime, 1, substate),

					Out = pre_gen_auth:authenticate("gerald", "herber"),
					?assertEqual({allow, "gerald"}, Out),
					?assert(meck:validate(superboy_prime)),

					meck:unload(superboy_prime),
					pre_gen_auth:remove_backend(superboy_prime, 1)
				end},

				{"get user", fun() ->
					ok = meck:new(alonzo_pierce, [non_strict]),
					meck:expect(alonzo_pierce, init,
						fun(substate) ->
							{ok, substate}
						end),
					meck:expect(alonzo_pierce, get_user,
						fun("gerald", substate) ->
							{user_auth, "gerald"}
						end),
					pre_gen_auth:add_backend(alonzo_pierce, 1, substate),

					Out = pre_gen_auth:get_user("gerald", alonzo_pierce, 1),
					?assertEqual({user_auth, "gerald"}, Out),
					meck:unload(alonzo_pierce),
					pre_gen_auth:remove_backend(alonzo_pierce, 1)
				end},

				{"faulty backend", fun() ->
					ok = meck:new(failmode, [non_strict]),
					meck:expect(failmode, init,
						fun(substate) ->
							{ok, substate}
						end),
					Self = self(),
					meck:expect(failmode, handle_authentication,
						fun("gerald", "herber", substate) ->
							Self ! truth,
							{error, the_pain}
						end),
					pre_gen_auth:add_backend(failmode, 1, substate),

					[?assertMatch({deny, _Msg}, pre_gen_auth:authenticate("gerald", "herber"))
						|| _ <- lists:seq(1, 4)],
					?assert(receive_truths(3)),

					meck:unload(failmode),
					pre_gen_auth:remove_backend(failmode, 1)
				end},

				{"backend fallthrough", fun() ->
					ok = meck:new([smartypants, know_nothing], [non_strict]),
					%meck:new(smartypants),
					meck:expect([know_nothing, smartypants], init,
						fun(substate) ->
							{ok, substate}
						end),
					pre_gen_auth:add_backend(know_nothing, 1, substate),
					pre_gen_auth:add_backend(smartypants, 2, substate),
					Self = self(),
					meck:expect(know_nothing, handle_authentication,
						fun("gerald", "herber", substate) ->
							Self ! truth,
							undefined
						end),
					meck:expect(smartypants, handle_authentication,
						fun("gerald", "herber", substate) ->
							Self ! truth,
							{allow, "gerald"}
						end),

					Out = pre_gen_auth:authenticate("gerald", "herber"),
					?assertEqual({allow, "gerald"}, Out),
					?assert(receive_truths(2)),

					pre_gen_auth:remove_backend(know_nothing, 1),
					pre_gen_auth:remove_backend(smartypants, 2),
					meck:unload(know_nothing),
					meck:unload(smartypants)
				end}
			]
		end}.

receive_truth() ->
	receive_truths(1).

receive_truths(0) -> true;
receive_truths(X) when X > 0 ->
	receive
		truth ->
			receive_truths(X - 1)
	after
		100 ->
			timeout
	end.

acting_as_backend_test_() ->
	{setup,
		fun() ->
			meck:new(mnesia)
		end,
		fun(_) ->
			meck:unload(mnesia)
		end,
		fun(_) ->
			[
				{"init", fun() ->
					Attrs = record_info(fields, user_auth),
					Self = self(),
					meck:expect(mnesia, create_table,
						fun(user_auth, [{attributes, InAttrs}]) ->
							?assertEqual(Attrs, InAttrs),
							Self ! truth,
							{atomic, ok}
						end),

					?assertEqual({ok, undefined}, pre_gen_auth:init(pre_gen_auth)),
					?assert(receive_truth()),
					?assert(meck:validate(mnesia))
				end},

				{"user not in mnesia", fun() ->
					Self = self(),
					meck:expect(mnesia, dirty_match_object,
						fun(_) ->
							Self ! truth,
							[]
						end),

					Out = pre_gen_auth:handle_authentication("gerald", "herber", undefined),
					?assert(receive_truth()),
					?assertEqual(undefined, Out)
				end},

				{"user in mnesia, bad pw", fun() ->
					Self = self(),
					meck:expect(mnesia, dirty_match_object,
						fun(_) ->
							Self ! truth,
							[#user_auth{password = "goodpass"}]
						end),

					Out = pre_gen_auth:handle_authentication("gerald", "badpass", undefined),
					?assert(receive_truth()),
					?assertEqual({deny, "invalid password"}, Out)
				end},

				{"user in mnesia everything's awesome", fun() ->
					Self = self(),
					meck:expect(mnesia, dirty_match_object,
						fun(_) ->
							Self ! truth,
							[#user_auth{password = "goodpass"}]
						end),

					Out = pre_gen_auth:handle_authentication("gerald", "goodpass", undefined),
					?assert(receive_truth()),
					?assertEqual({allow, "gerald"}, Out)
				end},

				{"get a user", fun() ->
					Expected = {user_auth, "gerald", "herber", []},
					Self = self(),
					meck:expect(mnesia, dirty_match_object,
						fun(_) ->
							Self ! truth,
							[Expected]
						end),

					Out = pre_gen_auth:get_user("gerald", undefined),
					?assert(receive_truth()),
					?assertEqual(Expected, Out)
				end}
			]
		end}.
