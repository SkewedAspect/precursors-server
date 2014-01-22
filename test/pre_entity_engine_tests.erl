-module(pre_entity_engine_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pre_entity.hrl").
-include("log.hrl").

-record(state, {
	entities = dict:new()
}).

general_test_() ->
	{setup, fun() ->
		meck:new(pre_entity_engine, [passthrough]),
		meck:expect(pre_entity_engine, init,
			fun([]) ->
			% Join the entity_engines process group.
				pg2:create(entity_engines),
				pg2:join(entity_engines, self()),

			% Join the entity_updates process group.
				pg2:create(entity_updates),
				pg2:join(entity_updates, self()),

				{ok, #state{}}
			end),
		{ok, PID} = gen_server:start_link(pre_entity_engine, [], []),
		PID
	end,
	fun(Pid) ->
		meck:unload(),
		unlink(Pid),
		exit(Pid, kill)
	end,
	fun(Pid) ->
		[
			{"Add Entity Test", fun() ->
				meck:new(pre_entity_engine_sup),
				meck:expect(pre_entity_engine_sup, cast_all, fun(_Thing1) -> abcast end),
				Return = pre_entity_engine:add_entity(Pid, #entity{id = <<"0">>}),
				?assertEqual(ok, Return),
				?assert(meck:validate(pre_entity_engine_sup)),
				meck:unload(pre_entity_engine_sup)
				end},
			{"Get Entity Test", fun() ->
				Return = pre_entity_engine:get_entity(Pid, <<"0">>),
				?assertEqual({ok, #entity{id = <<"0">>}}, Return)
				end},
			{"Get Invalid Entity Test", fun() ->
				Return = pre_entity_engine:get_entity(Pid, <<"1">>),
				?assertEqual({error, not_found}, Return)
			end
			},
			{"Remove Entity Test", fun() ->
				Return = pre_entity_engine:remove_entity(Pid, <<"0">>),
				?assertEqual(ok, Return),
				Return2 = pre_entity_engine:get_entity(Pid, <<"0">>),
				?assertEqual({error, not_found}, Return2)
			end},
			{"Remove Invalid Entity Test", fun() ->
				pre_entity_engine:add_entity(Pid, #entity{id = <<"0">>}),
				Return = pre_entity_engine:remove_entity(Pid, <<"1">>),
				?assertEqual(ok, Return),
				Return2 = pre_entity_engine:get_entity(Pid, <<"0">>),
				?assertEqual({ok, #entity{id = <<"0">>}}, Return2)
			end},
			%% TODO: Complete this test
			{"Simulate Entities No Update Test", fun() ->
				meck:new(nochange),
				meck:expect(nochange, simulate, fun(Entity, _State) -> {undefined, Entity} end),
				meck:new(change),
				meck:expect(change, simulate, fun(Entity, _State) -> {change, Entity#entity{state = [{}]}} end),
				TestEntity = #entity {id = <<"0">>, controller = nochange},
				TestEntity1 = #entity {id = <<"1">>, controller = change},
				TestState = #state {entities = dict:from_list([{<<"0">>, TestEntity}])},
				TestState1 = #state {entities = dict:from_list([{<<"1">>, TestEntity1}])},
				NewState = pre_entity_engine:simulate_entities(TestState),
				NewState1 = pre_entity_engine:simulate_entities(TestState1),
				?assert(meck:validate(nochange)),
				?assert(meck:validate(change)),
				?assertEqual(NewState, TestState),
				?assertNot(NewState1 =:= TestState1),
				meck:unload(nochange),
				meck:unload(change)
			end}
		]
	end}.
