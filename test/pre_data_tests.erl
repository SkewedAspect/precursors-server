-module(pre_data_tests).

-include_lib("eunit/include/eunit.hrl").

simple_start_test() ->
	Got = pre_data:start_link(fake_callback),
	?assertMatch({ok, _}, Got),
	{ok, Pid} = Got,
	?assert(is_pid(Pid)),
	pre_data:stop(),
	Mon = erlang:monitor(process, Pid),
	receive
		{'DOWN', Mon, process, Pid, _} ->
			ok
	end.

data_access_test_() ->
	{setup, fun() ->
		meck:new(data_callback, [non_strict]),
		{ok, Pid} = pre_data:start_link(data_callback),
		Pid
	end,
	fun(Pid) ->
		meck:unload(data_callback),
		pre_data:stop(),
		Mon = erlang:monitor(process, Pid),
		receive
			{'DOWN', Mon, process, Pid, _} ->
				ok
		end
	end,
	fun(_) ->
	
		SearchParams = [{key, value}, {key, '<', value}, {key, '>', value},
			{key, '==', value}, {key, '>=', value}, {key, '=<', value},
			{key, '=:=', value}, {key, member, [value]}],
		SearchParamTests = lists:map(fun(Param) ->
			Name = io_lib:format("search params test: ~p", [Param]),
			{iolist_to_binary(Name), ?_assertEqual({ok, []}, pre_data:search(goober, [Param]))}
		end, SearchParams),

		meck:expect(data_callback, search, fun(goober, _Whatevs) ->
			{ok, []}
		end),

		SearchParamTests ++ [


		{"get by id, all okay", fun() ->
			meck:expect(data_callback, get_by_id, fun(goober, 5) ->
				{ok, {goober, 5, <<"pants">>}}
			end),
			?assertEqual({ok, {goober, 5, <<"pants">>}}, pre_data:get_by_id(goober, 5))
		end},

		{"get by id, not found", fun() ->
			meck:expect(data_callback, get_by_id, fun(goober, 5) ->
				{error, notfound}
			end),
			?assertEqual({error, notfound}, pre_data:get_by_id(goober, 5))
		end},

		{"get by id, some other error", fun() ->
			meck:expect(data_callback, get_by_id, fun(goober, 5) ->
				{error, explosions}
			end),
			?assertEqual({error, explosions}, pre_data:get_by_id(goober, 5))
		end},

		{"save", fun() ->
			meck:expect(data_callback, save, fun({goober, undefined, <<"pants">>} = Tuple) ->
				{ok, setelement(2, Tuple, 1)}
			end),
			?assertEqual({ok, {goober, 1, <<"pants">>}}, pre_data:save({goober, undefined, <<"pants">>}))
		end},

		{"delete arity 2", fun() ->
			meck:expect(data_callback, delete, fun(goober, 3) ->
				{ok, 1}
			end),
			?assertEqual({ok, 1}, pre_data:delete(goober, 3))
		end},

		{"delete arity 1 becomes two", fun() ->
			meck:expect(data_callback, delete, fun(goober, 3) ->
				{ok, 1}
			end),
			?assertEqual({ok, 1}, pre_data:delete({goober, 3, <<"pants">>}))
		end},

		{"search", fun() ->
			meck:expect(data_callback, search, fun(goober, [{name, <<"hemdal">>}]) ->
				{ok, [{goober, 1, <<"hemdal">>}]}
			end),
			?assertEqual({ok, [{goober, 1, <<"hemdal">>}]}, pre_data:search(goober, [{name, <<"hemdal">>}]))
		end},

		{"search explosion on bad comparison", fun() ->
			?assertThrow({badarg, infix}, pre_data:search(goober, [{field, infix, 3}]))
		end},

		{"transations", fun() ->
			TransactFun = fun() ->
				pre_data:get_by_id(goober, 5)
			end,
			meck:expect(data_callback, transaction, fun(TransactionFun) ->
				TransactionFun()
			end),
			meck:expect(data_callback, get_by_id, fun(goober, 5) ->
				{ok, {goober, 5, <<"pants">>}}
			end),
			?assertEqual({ok, {goober, 5, <<"pants">>}}, pre_data:transaction(TransactFun))
		end},

		{"something throws an error", fun() ->
			meck:expect(data_callback, transaction, fun(TransactionFun) ->
				TransactionFun()
			end),
			TransactFun = fun() ->
				1 = 2
			end,
			Got = pre_data:transaction(TransactFun),
			?assertMatch({error, {{badmatch, 2}, _}}, Got)
		end}

	] end}.

