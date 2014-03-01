-module(pre_control_channel_tests).

-include("pre_client.hrl").
-include_lib("eunit/include/eunit.hrl").

channel_test_() ->
	% Setup
	{setup, fun() ->
		meck:new(pre_client)
	end,

	% Teardown
	fun(_) ->
		meck:unload()
	end,

	% Test runner
	fun(_) -> [
		{ "can login with valid credentials",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		},
		{ "denies login with invalid credentials",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		},
		{ "can get list of characters, if logged in",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		},
		{ "denies getting list of characters, if not logged in",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		},
		{ "can get select a character, if logged in",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		},
		{ "denies selecting a character, if not logged in",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		},
		{ "can log out, killing the client pid",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		}]
	end
	}.

