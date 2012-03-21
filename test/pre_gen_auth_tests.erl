-module(pre_gen_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").
-include("log.hrl").
%-include("internal_auth.hrl").

acting_as_manager_test_() ->
	{setup, fun() ->
		meck:new(mnesia),
		{ok, Pid} = pre_gen_auth:start_link([])
	end,
	fun(_) ->
		meck:unload(mnesia)
	end,
	fun(_) -> [

		{"User not found", fun() ->
			meck:expect(mnesia, transaction, fun(_) -> {atomic, []} end),
			Out = pre_gen_auth:authenticate("gerald", "herbert"),
			?assertEqual({deny, "No backends definitive"}, Out),
			?assert(meck:validate(mnesia))
		end},

		{"User found and denied", fun() ->
			meck:new(goober_back),
			meck:expect(goober_back, init, fun(substate) ->
				?info("init called"),
				{ok, substate}
			end),
			meck:expect(goober_back, handle_authentication, fun("gerald", "herber", substate) ->
				?info("Preparing for denial"),
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
			meck:new(superboy_prime),
			meck:expect(superboy_prime, init, fun(substate) ->
				{ok, substate}
			end),
			meck:expect(superboy_prime, handle_authentication, fun("gerald", "herber", substate) ->
				?info("preparing for allowage"),
				allow
			end),
			pre_gen_auth:add_backend(superboy_prime, 1, substate),
			Out = pre_gen_auth:authenticate("gerald", "herber"),
			?assertEqual(allow, Out),
			?assert(meck:validate(superboy_prime)),
			meck:unload(superboy_prime),
			pre_gen_auth:remove_backend(superboy_prime, 1)
		end},

		{"get user", fun() ->
			meck:new(alonzo_pierce),
			meck:expect(alonzo_pierce, init, fun(substate) ->
				{ok, substate}
			end),
			meck:expect(alonzo_pierce, get_user, fun("gerald", substate) ->
				{user_auth, "gerald"}
			end),
			pre_gen_auth:add_backend(alonzo_pierce, 1, substate),
			Out = pre_gen_auth:get_user("gerald", alonzo_pierce, 1),
			?assertEqual({user_auth, "gerald"}, Out),
			meck:unload(alonzo_pierce)
		end}

	] end}.

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
	{setup, fun() ->
		meck:new(mnesia)
	end,
	fun(_) ->
		meck:unload(mnesia)
	end,
	fun(_) -> [

		{"init", fun() ->
			Attrs = record_info(fields, user_auth),
			Self = self(),
			meck:expect(mnesia, create_table, fun(user_auth, [{attributes,
				InAttrs}]) ->
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
			meck:expect(mnesia, dirty_match_object, fun(_) ->
				Self ! truth,
				[]
			end),
			Out = pre_gen_auth:handle_authentication("gerald", "herber", undefined),
			?assert(receive_truth()),
			?assertEqual(undefined, Out)
		end},

		{"user in mnesia, bad pw", fun() ->
			Self = self(),
			meck:expect(mnesia, dirty_match_object, fun(_) ->
				Self ! truth,
				[#user_auth{password = "goodpass"}]
			end),
			Out = pre_gen_auth:handle_authentication("gerald", "badpass", undefined),
			?assert(receive_truth()),
			?assertEqual({deny, "invalid password"}, Out)
		end},

		{"user in mnesia everything's awesome", fun() ->
			Self = self(),
			meck:expect(mnesia, dirty_match_object, fun(_) ->
				Self ! truth,
				[#user_auth{password = "goodpass"}]
			end),
			Out = pre_gen_auth:handle_authentication("gerald", "goodpass", undefined),
			?assert(receive_truth()),
			?assertEqual(allow, Out)
		end},

		{"get a user", fun() ->
			Expected = {user_auth, "gerald", "herber", []},
			Self = self(),
			meck:expect(mnesia, dirty_match_object, fun(Yar) ->
				Self ! truth,
				[Expected]
			end),
			Out = pre_gen_auth:get_user("gerald", undefined),
			?assert(receive_truth()),
			?assertEqual(Expected, Out)
		end}

	] end}.
