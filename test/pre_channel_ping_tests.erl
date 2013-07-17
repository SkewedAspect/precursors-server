-module(pre_channel_ping_tests).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("pre_client.hrl").

pre_channel_ping_test_() ->
	[
		{"Register Hooks Test",
			fun() ->
				meck:new(pre_hooks),
				meck:expect(pre_hooks, add_hook,
					fun(client_logged_in, pre_channel_ping, client_login_hook, undefined, Nodes) ->
						TestNodes = [node()],
						?assertEqual(TestNodes, Nodes)
					end),

				pre_channel_ping:register_hooks(),
				History = meck:history(pre_hooks),
				?assertEqual(1, length(History)),

				meck:unload(pre_hooks)
			end},

		{"Client Login Hook Trigger Test",
			fun() ->
				meck:new(pre_client_channels),
				TestRecord = #client_info{channel_manager = <<"TestString">>},
				meck:expect(pre_client_channels, set_channel,
					fun(<<"TestString">>, <<"ping">>, pre_channel_ping, []) ->
						ok
					end),

				pre_channel_ping:client_login_hook(undefined, TestRecord),
				History = meck:history(pre_client_channels),
				?assertEqual(1, length(History)),

				meck:unload(pre_client_channels)
			end},

		{"Client Request/5 Test",
			fun() ->
				meck:new(pre_client_connection),
				meck:expect(pre_client_connection, send,
					fun(Connection, tcp, {response, Id}, <<"ping">>, PingResponse) ->
						{Mega, Secs, _} = now(),
						TestTimeStamp = Mega * 1000000 + Secs,
						InTimeStamp = proplists:get_value(timestamp, PingResponse),
						?assert(InTimeStamp - TestTimeStamp =< 1),
						?debug("Stamp difference: ~p", [InTimeStamp - TestTimeStamp]),
						ok
					end),
				TestRecord = #client_info{connection = <<"TestString">>},

				pre_channel_ping:client_request(<<"ping">>, TestRecord, 5, undefined, undefined),
				History = meck:history(pre_client_connection),
				?assertEqual(1, length(History)),

				meck:unload(pre_client_connection)
			end}
	].
