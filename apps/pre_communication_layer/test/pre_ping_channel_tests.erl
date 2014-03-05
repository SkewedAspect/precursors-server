-module(pre_ping_channel_tests).

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
		{ "responds to pings",
			fun() ->
				meck:expect(pre_client, send_response, fun(_Pid, Channel, ID, PingResponse) ->
					ExpChannel = <<"ping">>,
					ExpID = <<"some_id">>,
					ExpConfirm = true,
					?assertEqual(ExpChannel, Channel),
					?assertEqual(ExpID, ID),
					?assertEqual(ExpConfirm, proplists:get_value(confirm, PingResponse))
				end),
				State = #client_state{},
				pre_ping_channel:handle_request(<<"ping">>, <<"some_id">>, fake_request, State),
				?assert(meck:validate(pre_client))
			end
		}]
	end
	}.

