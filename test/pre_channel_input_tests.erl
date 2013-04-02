-module(pre_channel_input_tests).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("pre_entity.hrl").


pre_channel_input_test_() ->
	[
		{"Register Hooks Test",
			fun() ->
				meck:new(pre_hooks),
				meck:expect(pre_hooks, add_hook,
					fun(client_logged_in, pre_channel_input, client_login_hook, undefined, Nodes) ->
						TestNodes = [node()],
						?assertEqual(TestNodes, Nodes)
					end),

				pre_channel_input:register_hooks(),
				History = meck:history(pre_hooks),
				?assertEqual(1, length(History)),

				meck:unload(pre_hooks)
			end},

		{"Client Login Hook Trigger Test",
			fun() ->
				meck:new(pre_client_channels),
				TestRecord = #client_info{channel_manager = <<"TestString">>},
				meck:expect(pre_client_channels, set_channel,
					fun(<<"TestString">>, <<"input">>, pre_channel_input, []) ->
						ok
					end),

				pre_channel_input:client_login_hook(undefined, TestRecord),
				History = meck:history(pre_client_channels),
				?assertEqual(1, length(History)),

				meck:unload(pre_client_channels)
            end}
	].


pre_channel_input_request_test_() ->
	{setup,
		fun() ->
			meck:new(pre_entity_engine, [passthrough]),
			meck:new(pre_entity_engine_sup)
		end,
		fun(_) ->
			meck:unload(pre_entity_engine_sup),
			meck:unload(pre_entity_engine)
		end,
		fun(_) ->
			[
				{"Client request",
					fun() ->
						meck:expect(pre_entity_engine, init,
							fun(substate) ->
								{ok, substate}
							end),
						meck:expect(pre_entity_engine, handle_call,
							fun({request, _EntityID, _Channel, _RequestType, _RequestID, _Request}, _From, State) ->
								Response = {reply, [
									{confirm, false},
									{reason, <<"VALID CRAPBACK: Invalid request!">>}
								]},
								{reply, {Response, #entity{}}, State}
							end),
						meck:expect(pre_entity_engine, handle_cast,
							fun(_Request, State) ->
								{noreply, State}
							end),
						meck:expect(pre_entity_engine, handle_info,
							fun(_Info, State) ->
								{noreply, State}
							end),
						meck:expect(pre_entity_engine, terminate,
							fun(_Reason, _State) ->
								whatever
							end),
						meck:expect(pre_entity_engine, code_change,
							fun(_OldVsn, _State, _Extra) ->
								{error, ni}
							end),

						{ok, TestEntityEnginePid} = gen_server:start_link(pre_entity_engine, substate, []),
						TestEntityID = <<"TestEntity 1">>,
						TestClient = #client_info{
							entity = TestEntityID,
							entity_engine = TestEntityEnginePid
						},
						TestRequest = [{type, <<"TestType">>}],

						meck:expect(pre_entity_engine_sup, get_entity_engine,
							fun(EntityID) ->
								TestEntityID = EntityID,
								{ok, TestEntityEnginePid}
							end),

						Response = pre_channel_input:client_request(TestClient, 1, TestRequest, undefined),
						ExpectedResponse = {
							{reply, [
								{confirm, false},
								{reason, <<"VALID CRAPBACK: Invalid request!">>}
							]},
							#entity{}
						},
						?assertEqual(ExpectedResponse, Response)
					end}
			]
		end}.
