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

		{"Client Request Test",
			fun() ->
				meck:new(pre_entity),
				meck:expect(pre_entity, init,
					fun(substate) ->
						{ok, substate}
					end),
				meck:expect(pre_entity, handle_call,
					fun({client_request, EntityID, ClientInfo, Channel, RequestType, RequestID, Request}, _From, State) ->
						Response = {reply, [
							{confirm, false},
							{reason, <<"VALID CRAPBACK: Invalid request!">>}
						]},
						{reply, {Response, #entity{}}, State}
					end),
				meck:expect(pre_entity, handle_cast,
					fun(_Request, State) ->
						{noreply, State}
					end),
				meck:expect(pre_entity, handle_info,
					fun(_Info, State) ->
						{noreply, State}
					end),
				meck:expect(pre_entity, terminate,
					fun(_Reason, State) ->
						whatever
					end),
				meck:expect(pre_entity, code_change,
					fun(_OldVsn, _State, _Extra) ->
						{error, ni}
					end),
				meck:expect(pre_entity, client_request,
					fun(RequestType, _EntityID, _RequestID, _Request) ->
						?assert(RequestType =:= <<"TestType">>),
						ok
					end),
				{ok, TestEntityPid} = gen_server:start_link(pre_entity, substate, []),
				TestEntityID = #entity_id{engine = TestEntityPid},
				TestClient = #client_info{entity = TestEntityID},
				TestRequest = [{type, <<"TestType">>}],

				Response = pre_channel_input:client_request(TestClient, undefined, TestRequest, undefined),
				ExpectedResponse = {
					{reply, [
						{confirm, false},
						{reason, <<"VALID CRAPBACK: Invalid request!">>}
					]},
					#entity{}
				},
				?assertEqual(ExpectedResponse, Response),

				meck:unload(pre_entity)
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
