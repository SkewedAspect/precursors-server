-module(pre_entity_engine_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pre_entity.hrl").

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
			end},

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
			end}
		]
	end}.
