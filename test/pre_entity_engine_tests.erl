-module(pre_entity_engine_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pre_entity.hrl").
-include("log.hrl").

-record(state, {
	entities = dict:new()
}).

general_test_() ->
	{setup, fun() ->
		{ok, PID} = pre_entity_engine:start_link(),
		{ok, SupPid} = pre_entity_engine_sup:start_link([]),
		{PID, SupPid}
	end,
	fun({Pid, SupPid}) ->
		unlink(Pid),
		unlink(SupPid),
		exit(Pid, kill),
		exit(SupPid, kill)
	end,
	fun({Pid, _SupPid}) ->
		[
			{"Add Entity Test", fun() ->
				Return = pre_entity_engine:add_entity(Pid, #entity{id = <<"0">>}),
				?assertEqual(ok, Return)
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
			end}%,
			%% TODO: Complete this test
%			{"Simulate Entities No Update Test", fun() ->
%				meck:new(nochange),
%				meck:expect(nochange, simulate, fun(Entity, _State) -> {undefined, Entity} end),
%				meck:new(change),
%				meck:expect(change, simulate, fun(Entity, _State) -> {change, Entity#entity{state = [{}]}} end),
%				TestEntity = #entity {id = <<"0">>, controller = nochange},
%				TestEntity1 = #entity {id = <<"1">>, controller = change},
%				TestState = #state {entities = dict:from_list([{<<"0">>, TestEntity}])},
%				TestState1 = #state {entities = dict:from_list([{<<"1">>, TestEntity1}])},
%				NewState = pre_entity_engine:simulate_entities(TestState),
%				NewState1 = pre_entity_engine:simulate_entities(TestState1),
%				?assert(meck:validate(nochange)),
%				?assert(meck:validate(change)),
%				?assertEqual(NewState, TestState),
%				?assertNot(NewState1 =:= TestState1),
%				meck:unload(nochange),
%				meck:unload(change)
%			end}
		]
	end}.
