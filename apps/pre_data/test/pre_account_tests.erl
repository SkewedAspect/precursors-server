%%%-------------------------------------------------------------------
%%% @author travis
%%% @doc
%%% Tests for pre_account
%%% @end
%%% Created : 04. Mar 2014 7:37 PM
%%%-------------------------------------------------------------------
-module(pre_account_tests).

-include_lib("eunit/include/eunit.hrl").

account_access_test_() ->
	{setup, fun() ->
		mnesia:delete_schema([node()]),
%% 		mnesia:delete({pre_rec_account, <<"test@test.com">>}),
		{ok, _} = application:ensure_all_started(pre_data),
		pre_mnesia:check_setup()
	end,
		fun(_) ->
			application:stop(pre_data)
		end,
		fun(_) -> [
			{"create account", fun() ->
				Got = pre_account:create(<<"test@test.com">>, <<"Aeon">>, <<"LizardMan">>, <<"12345">>),
				?assertMatch({ok, _}, Got)
			end
			},
			{"can't create duplicate account", fun() ->
				Got1 = pre_account:create(<<"dupe@test.com">>, <<"Aeon">>, <<"LizardMan">>, <<"12345">>),
				Got2 = pre_account:create(<<"dupe@test.com">>, <<"Aeon">>, <<"LizardMan">>, <<"12345">>),
				?assertMatch({ok, _}, Got1),
				?assertMatch({error, already_exists}, Got2)
			end
			},
			{"look up account by email", fun() ->
				Got = pre_account:get_by_email(<<"test@test.com">>),
				?assertMatch({ok, _}, Got)
			end
			},
			{"look up account by id", fun() ->
				Got = pre_account:get_by_id(1),
				?assertMatch({ok, _}, Got)
			end
			},
			{"autheticate account with password", fun() ->
				Got = pre_account:authenticate(<<"test@test.com">>, <<"12345">>),
				?assertMatch(ok, Got)
			end
			},
			{"delete account", fun() ->
				ok = pre_account:delete(1),
				?assertMatch({error, notfound}, pre_account:get_by_id(1))
			end
			}
		] end }.
