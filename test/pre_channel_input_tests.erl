-module(pre_channel_input_tests).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("pre_client.hrl").

pre_channel_input_test_()->
	[{"Register Hooks Test",
				fun()->
					meck:new(pre_hooks),
					meck:expect(pre_hooks, add_hook, fun(client_logged_in, pre_channel_input, client_login_hook, undefined, Nodes)->
								TestNodes = [node()],
								?assertEqual(TestNodes, Nodes)
						end),
					pre_channel_input:register_hooks(),
					History = meck:history(pre_hooks),
					?assertEqual(1, length(History)),
					meck:unload(pre_hooks)
			end},
		{"Client Request Test",
			fun()->
					meck:new(pre_entity),
					meck:expect(pre_entity, client_request, fun(RequestType, EntityID, RequestID, Request)->
                                ?assert(RequestType =:= <<"TestType">>),
								ok
						end),
                    TestClient = #client_info{entity = <<"TestEntity">>},
                    TestRequest = {struct, [{<<"type">>, <<"TestType">>}]},
					pre_channel_input:client_request(TestClient, undefined, TestRequest, undefined),
                    History = meck:history(pre_entity),
                    ?assertEqual(1, length(History)),
                    meck:unload(pre_entity)
            end},
		{"Client Login Hook Trigger Test",
			fun()->
					meck:new(pre_client_channels),
					TestRecord = #client_info{channel_manager = <<"TestString">>},
					meck:expect(pre_client_channels, set_channel, fun(<<"TestString">>, <<"input">>, pre_channel_input, [])->
							ok
					end),
				pre_channel_input:client_login_hook(undefined, TestRecord),
				History = meck:history(pre_client_channels),
				?assertEqual(1, length(History)),
				meck:unload(pre_client_channels)
            end}
		].
