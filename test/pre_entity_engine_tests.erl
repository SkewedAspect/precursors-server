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
					meck:expect(pre_entity_engine_sup, cast_all, fun(Thing1) -> abcast end),
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
                end}

            ]
        end}.