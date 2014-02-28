-module(pre_entity_channel_tests).

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
		{ "can handle requests for full updates",
			fun() ->
				%TODO: Implement this test, when the functionality is finished.
				?assert(true)
			end
		},
		{ "denies requests for full updates if we don't have an entity",
			fun() ->
				meck:expect(pre_client, send_response, fun(_Pid, Channel, ID, Response) ->
					ExpChannel = <<"entity">>,
					ExpID = <<"some_id">>,
					ExpConfirm = false,
					?assertEqual(ExpChannel, Channel),
					?assertEqual(ExpID, ID),
					?assertEqual(ExpConfirm, proplists:get_value(confirm, Response))
				end),
				State = #client_state{},
				pre_ping_channel:handle_request(<<"full">>, <<"some_id">>, fake_request, State),
				?assert(meck:validate(pre_client))
			end
		}]
	end
	}.

