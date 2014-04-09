%%%-------------------------------------------------------------------
%%% @author travis
%%% @doc
%%% Tests for pre_character
%%% @end
%%% Created : 04. Mar 2014 7:37 PM
%%%-------------------------------------------------------------------
-module(pre_character_tests).

-include_lib("eunit/include/eunit.hrl").

character_test_() ->
	{setup, fun() ->
		mnesia:delete_schema([node()]),
		{ok, _} = application:ensure_all_started(pre_data),
		pre_mnesia:check_setup()
	end,
		fun(_) ->
			application:stop(pre_data)
		end,
		fun(_) -> [
			{"create character", fun() ->
				% Name, Account, Race, Faction, Ship, Level
				Got = pre_character:create(<<"Test Char">>, 1, human, league, 1, 10),
				?assertMatch({ok, _}, Got)
			end
			},
			{"can't create duplicate character", fun() ->
				Got1 = pre_character:create(<<"dupe">>, 2, human, league, 1, 10),
				Got2 = pre_character:create(<<"dupe">>, 2, human, league, 1, 10),
				?assertMatch({ok, _}, Got1),
				?assertMatch({error, already_exists}, Got2)
			end
			},
			{"look up character by account", fun() ->
				Got = pre_character:get_by_account(1),
				?assertMatch({ok, [_]}, Got),
				{ok, Results} = Got,
				?assertEqual(1, length(Results))
			end
			},
			{"look up character by name", fun() ->
				Got = pre_character:get_by_name(<<"Test Char">>),
				?assertMatch({ok, _}, Got)
			end
			},
			{"look up non-existant character by name", fun() ->
				Got = pre_character:get_by_name(<<"does not exist">>),
				?assertMatch({error, notfound}, Got)
			end
			},
			{"look up duplicate character by name", fun() ->
				pre_character:create(<<"dupe">>, 2, human, league, 1, 10),

				% Create by hand, to test the duplicate error state.
				New = pre_rec_character:new(<<"dupe">>, 2, human, league, 1, 10),
				pre_data:transaction(fun() -> pre_data:save(New) end),

				Got = pre_character:get_by_name(<<"dupe">>),
				?assertMatch({error, duplicates}, Got)
			end
			},
			{"look up character by id", fun() ->
				Got = pre_character:get_by_id(1),
				?assertMatch({ok, _}, Got)
			end
			},
			{"delete character", fun() ->
				ok = pre_character:delete(1),
				?assertMatch({error, notfound}, pre_character:get_by_id(1))
			end
			}
		] end }.
