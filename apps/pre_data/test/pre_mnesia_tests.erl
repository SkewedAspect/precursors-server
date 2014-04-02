-module(pre_mnesia_tests).

-include_lib("eunit/include/eunit.hrl").

-record(pre_thing, {
	id, val, created = os:timestamp(), updated = os:timestamp()
}).

setup_test_() -> [
	{"clean from previous runs", fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()])
	end},

	{"able to start", fun() ->
		mnesia:start(),
		Got = pre_mnesia:check_setup(),
		?assertEqual(ok, Got)
	end},

	{"counter update", fun() ->
		Got2 = mnesia:dirty_update_counter(pre_counters, goober, 1),
		?assertEqual(1, Got2)
	end},

	{"has pre_rec_account table", fun() ->
		Fields = pre_rec_account:field_names(),
		GotFields = mnesia:table_info(pre_rec_account, attributes),
		?assertEqual(Fields, GotFields)
	end},

	{"pre_rec_account table is a disc available copy", fun() ->
		DiscCopies = mnesia:table_info(pre_rec_account, disc_copies),
		?assert(lists:member(node(), DiscCopies))
	end},

	{"persist through mnesia restart", fun() ->
		mnesia:stop(),
		mnesia:start(),
		DiscCopies = mnesia:table_info(pre_rec_account, disc_copies),
		?assert(lists:member(node(), DiscCopies))
	end},

	{"teardown", fun() ->
		mnesia:stop(),
		mnesia:delete_schema([node()])
	end}

	].

non_persist_setup_test_() -> [

	{"able to start", fun() ->
		mnesia:start(),
		Got = pre_mnesia:check_setup(false),
		?assertEqual(ok, Got)
	end},

	{"counter update", fun() ->
		Got = mnesia:dirty_update_counter(pre_counters, goober, 1),
		?assertEqual(1, Got)
	end},

	{"no persistence through mneisa restart", fun() ->
		mnesia:stop(),
		mnesia:start(),
		?assertExit({aborted, {combine_error, pre_counters, update_counter}}, mnesia:dirty_update_counter(pre_counters, goober, 1))
	end},

	{"teardown", fun() ->
		mnesia:stop()
	end}

	].

data_access_test_() ->
	% yes I know all these tests are order dependant. I don't care right now.
	{setup, fun() ->
		%mnesia:create_schema([node()]),
		mnesia:start(),
		pre_mnesia:check_setup(),
		mnesia:create_table(pre_thing, [
			{attributes, [id, val, created, updated]}
		]),
		{ok, Pid} = pre_data:start_link(pre_mnesia),
		Pid
	end,
	fun(_Pid) ->
		pre_data:stop(),
		mnesia:stop(),
		mnesia:delete_schema([node()])
	end,
	fun(_) -> [

		{"simple transaction", fun() ->
			Got = pre_data:transaction(fun() -> ok end),
			?assertEqual(ok, Got)
		end},

		{"can save without an id", fun() ->
			Input = #pre_thing{val = <<"a val">>},
			Got = pre_data:transaction(fun() ->
				Out = pre_data:save(Input),
				Out
			end),
			?assertMatch({ok, _}, Got),
			{ok, GotRec} = Got,
			?assert(is_tuple(GotRec)),
			?assertNotEqual(Input#pre_thing.id, GotRec#pre_thing.id),
			ExpectRec = Input#pre_thing{id = GotRec#pre_thing.id},
			?assertEqual(ExpectRec, GotRec)
		end},

		{"can get by an id", fun() ->
			Got = pre_data:transaction(fun() ->
				pre_data:get_by_id(pre_thing, 1)
			end),
			?assertMatch({ok, _}, Got)
		end},

		{"can search", fun() ->
			Got = pre_data:transaction(fun() ->
				pre_data:search(pre_thing, [{val, <<"a val">>}])
			end),
			?assertMatch({ok, [_]}, Got)
		end},

		{"can delete", fun() ->
			Got = pre_data:transaction(fun() ->
				pre_data:delete(pre_thing, 1)
			end),
			?assertEqual(ok, Got),
			Getted = pre_data:transaction(fun() ->
				pre_data:get_by_id(pre_thing, 1)
			end),
			?assertEqual({error, notfound}, Getted)
		end},

		{"search with no results", fun() ->
			Got = pre_data:transaction(fun() ->
				pre_data:search(pre_thing, [{val, <<"a val">>}])
			end),
			?assertEqual({ok, []}, Got)
		end}

	] end}.

