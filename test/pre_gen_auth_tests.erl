-module(pre_gen_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").

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
			?assertEqual(undefined, Out),
			?assert(meck:validate(mnesia))
		end}

	] end}.

acting_as_manager_test_() ->
	[?_assert(false)].
