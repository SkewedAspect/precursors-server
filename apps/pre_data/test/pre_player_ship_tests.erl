-module(pre_player_ship_tests).

-include_lib("eunit/include/eunit.hrl").

player_ship_test_() ->
	{setup, fun() ->
		mnesia:delete_schema([node()]),
		{ok, _} = application:ensure_all_started(pre_data),
		pre_mnesia:check_setup()
	end,
		fun(_) ->
			application:stop(pre_data)
		end,
		fun(_) -> [
			{"create player_ship", fun() ->
				% Name, Template
				Got = pre_player_ship:create(<<"U.S.S Enterprise">>, <<"Test Ship">>),
				?assertMatch({ok, _}, Got)
			end
			},
			{"look up player_ship by name", fun() ->
				Got = pre_player_ship:get_by_name(<<"U.S.S Enterprise">>),
				?assertMatch({ok, _}, Got)
			end
			},
			{"look up non-existant player_ship by name", fun() ->
				Got = pre_player_ship:get_by_name(<<"Mistake Not...">>),
				?assertMatch({error, notfound}, Got)
			end
			},
			{"look up duplicate player_ship by name", fun() ->
				pre_player_ship:create(<<"Mistake Not...">>, <<"Test Ship">>),
				pre_player_ship:create(<<"Mistake Not...">>, <<"Test Ship">>),
				Got = pre_player_ship:get_by_name(<<"Mistake Not...">>),
				?assertMatch({ok, [_ | _]}, Got)
			end
			},
			{"look up player_ship by id", fun() ->
				Got = pre_player_ship:get_by_id(1),
				?assertMatch({ok, _}, Got)
			end
			},
			{"delete player_ship", fun() ->
				ok = pre_player_ship:delete(1),
				?assertMatch({error, notfound}, pre_player_ship:get_by_id(1))
			end
			}
		] end }.
