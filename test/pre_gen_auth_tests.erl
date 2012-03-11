-module(pre_gen_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").
-include("log.hrl").

acting_as_backend_test_() ->
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
		end}

	] end}.

acting_as_manager_test_() ->
	[?_assert(false)].
