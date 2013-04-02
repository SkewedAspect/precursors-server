-module(pre_entity_engine_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pre_entity.hrl").
-include("log.hrl").

general_test_() ->
    {setup, fun() ->
        {ok, PID} = meck:new(pre_entity_engine, [passthrough]),
        PID
    end,
     fun(Pid) ->
            unlink(Pid),
            exit(Pid, kill)
        end,
        fun(Pid) ->
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
                end}

            ]
        end}.