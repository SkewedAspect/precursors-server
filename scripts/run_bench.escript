#!/usr/bin/env escript
%%! -noshell -noinput -smp enable

%% Benchmarking key/value data types
%%
%% Running:
%%
%%   $ erlc -smp gen_bench.erl
%%   $ escript run_bench.escript

-mode(compile).

-compile(export_all).

-define(ITERS, 10000).
-define(REPS, 10).

%% --------------------------------------------------------------------------------------------------------------------

main([]) ->
	lists:map(fun ?MODULE:run_gen_bench/1, [
		{fun ?MODULE:baseline/1, ignore_me}
		]),

	Tests = [
		pdict,
		dict,
		ets,
		proplist
	],
	Sizes = [
		1,
		10,
		100,
		1000,
		10000,
		100000
	],
	lists:map(fun ?MODULE:run_gen_bench/1, [
		{fun ?MODULE:Test/1, Size}
		|| Size <- Sizes, Test <- Tests
	]).

%% --------------------------------------------------------------------------------------------------------------------

run_gen_bench({Fun, Arg}) ->
	gen_bench:start_link(),
	Fun(Arg),
	gen_bench:stop().

%% --------------------------------------------------------------------------------------------------------------------

baseline(ignore_me) ->
	run_benches(
		"Baseline",
		[
			{basic_operation, fun ?MODULE:null/0, fun ?MODULE:null/1},
			{operation_with_generated_key, fun ?MODULE:null/0, fun ?MODULE:null_gen_key/1}
		],
		?ITERS, ?REPS
	).

dict(Size) ->
	SetupFunc = fun() ->
		Dict = dict:new(),
		{
			Size,
			lists:foldl(
				fun(Key, LastDict) ->
					dict:store(list_to_binary(integer_to_list(Key)), <<"Value ", Key/integer>>, LastDict)
				end,
				Dict,
				lists:seq(1, Size)
			)
		}
	end,
	run_benches(
		io_lib:format("dict (~p items)", [Size]),
		[
			{create, fun ?MODULE:null/0, fun(ok) -> dict:new() end},
			{fetch, SetupFunc, fun ?MODULE:dict_fetch/1},
			{iterate_map, SetupFunc, fun ?MODULE:dict_iterate_map/1},
			{iterate_fold, SetupFunc, fun ?MODULE:dict_iterate_fold/1},
			{to_list, SetupFunc, fun ?MODULE:dict_to_list/1},
			{store, SetupFunc, fun ?MODULE:dict_store/1},
			{erase, SetupFunc, fun ?MODULE:dict_erase/1}
		],
		?ITERS, ?REPS
	).

pdict(Size) ->
	SetupFunc = fun() ->
		[
			put({ourshit, list_to_binary(integer_to_list(Key))}, <<"Value ", Key/integer>>)
			|| Key <- lists:seq(1, Size)
		],
		{Size}
	end,
	run_benches(
		io_lib:format("Process dictionary (~p items)", [Size]),
		[
			{get, SetupFunc, fun ?MODULE:pdict_get/1},
			{iterate_list_comp, SetupFunc, fun ?MODULE:pdict_iterate_list_comp/1},
			{iterate_map, SetupFunc, fun ?MODULE:pdict_iterate_map/1},
			{iterate_foldl, SetupFunc, fun ?MODULE:pdict_iterate_foldl/1},
			{to_list_list_comp, SetupFunc, fun ?MODULE:pdict_to_list_list_comp/1},
			{store, SetupFunc, fun ?MODULE:pdict_store/1},
			{erase, SetupFunc, fun ?MODULE:pdict_erase/1}
		],
		?ITERS, ?REPS
	).

ets(Size) ->
	SetupFunc = fun() ->
		EtsTable = ets:new(whatthefuck, [private]),
		[
			ets:insert(EtsTable, {list_to_binary(integer_to_list(Key)), <<"Value ", Key/integer>>})
			|| Key <- lists:seq(1, Size)
		],
		{Size, EtsTable}
	end,
	run_benches(
		io_lib:format("ets (~p items)", [Size]),
		[
			{lookup, SetupFunc, fun ?MODULE:ets_lookup/1},
			%{iterate_foldl, SetupFunc, fun ?MODULE:ets_iterate_foldl/1}, % HORRIBLE
			%{iterate_match, SetupFunc, fun ?MODULE:ets_iterate_match/1}, % Not as bad, but still BAD
			{iterate_select, SetupFunc, fun ?MODULE:ets_iterate_select/1},
			{iterate_first_next, SetupFunc, fun ?MODULE:ets_iterate_first_next/1},
			{to_list_match, SetupFunc, fun ?MODULE:ets_to_list_match/1},
			{to_list_select, SetupFunc, fun ?MODULE:ets_to_list_select/1},
			{insert, SetupFunc, fun ?MODULE:ets_insert/1},
			{delete, SetupFunc, fun ?MODULE:ets_delete/1}
		],
		?ITERS, ?REPS
	).

proplist(Size) ->
	SetupFunc = fun() ->
		Proplist = [
			{list_to_binary(integer_to_list(Key)), <<"Value ", Key/integer>>}
			|| Key <- lists:seq(1, Size)
		],
		{Size, Proplist}
	end,
	run_benches(
		io_lib:format("proplist (~p items)", [Size]),
		[
			{get_value, SetupFunc, fun ?MODULE:proplist_get_value/1},
			{lookup, SetupFunc, fun ?MODULE:proplist_lookup/1},
			{iterate_list_comp, SetupFunc, fun ?MODULE:proplist_iterate_list_comp/1},
			{iterate_map, SetupFunc, fun ?MODULE:proplist_iterate_map/1},
			{iterate_foldl, SetupFunc, fun ?MODULE:proplist_iterate_foldl/1},
			{keystore, SetupFunc, fun ?MODULE:proplist_keystore/1},
			{delete, SetupFunc, fun ?MODULE:proplist_delete/1}
		],
		?ITERS, ?REPS
	).

%% --------------------------------------------------------------------------------------------------------------------

%% Empty function used for the 'base_case/2' benchmark. It must do nothing interesting.
null() -> ok.
null(_) -> ok.
null(_, _) -> ok.

null_gen_key(_) -> list_to_binary(integer_to_list(random:uniform(10))).

%% --------------------------------------------------------------------------------------------------------------------

dict_fetch({Size, Dict}) ->
	dict:fetch(list_to_binary(integer_to_list(random:uniform(Size))), Dict).

dict_iterate_map({_Size, Dict}) ->
	dict:map(fun(_Key, Value) -> null(Value) end, Dict).

dict_iterate_fold({_Size, Dict}) ->
	dict:fold(fun(_Key, Value, _) -> null(Value) end, ok, Dict).

dict_to_list({_Size, Dict}) ->
	dict:to_list(Dict).

dict_store({Size, Dict}) ->
	dict:store(list_to_binary(integer_to_list(random:uniform(Size))), <<"DUMMY VALUE">>, Dict).

dict_erase({Size, Dict}) ->
	dict:erase(list_to_binary(integer_to_list(random:uniform(Size))), Dict).

%% --------------------------------------------------------------------------------------------------------------------

pdict_get({Size}) ->
	get({ourshit, list_to_binary(integer_to_list(random:uniform(Size)))}).

pdict_iterate_list_comp({_Size}) ->
	[
		begin
			{_Key, Value} = Item,
			null(Value)
		end
		|| Item <- get(), is_record(Item, ourshit, 1)
	].

pdict_iterate_map({_Size}) ->
	lists:map(fun({{ourshit, _Key}, Value}) -> null(Value); (_) -> nothing end, get()).

pdict_iterate_foldl({_Size}) ->
	lists:foldl(fun({{ourshit, _Key}, Value}, _) -> null(Value); (_, _) -> nothing end, ok, get()).

pdict_to_list_list_comp({_Size}) ->
	[
		Item
		|| Item <- get(), is_record(Item, ourshit, 1)
	].

pdict_store({Size}) ->
	put({ourshit, list_to_binary(integer_to_list(random:uniform(Size)))}, <<"DUMMY VALUE">>).

pdict_erase({Size}) ->
	erase({ourshit, list_to_binary(integer_to_list(random:uniform(Size)))}).

%% --------------------------------------------------------------------------------------------------------------------

ets_lookup({Size, EtsTable}) ->
	ets:lookup(EtsTable, list_to_binary(integer_to_list(random:uniform(Size)))).

ets_iterate_foldl({_Size, EtsTable}) ->
	ets:foldl(fun ?MODULE:null/2, [], EtsTable).

ets_iterate_match({_Size, EtsTable}) ->
	[null(Item) || Item <- ets:match(EtsTable, '$1')].

ets_iterate_select({_Size, EtsTable}) ->
	[null(Item) || Item <- ets:select(EtsTable, [{'$1', [], ['$_']}])].

ets_iterate_first_next({_Size, EtsTable}) ->
	ets_iterate_first_next_process(ets:first(EtsTable), EtsTable).

ets_iterate_first_next_process('$end_of_table', _EtsTable) ->
	ok;

ets_iterate_first_next_process(Key, EtsTable) ->
	null(ets:lookup(EtsTable, Key)),
	ets_iterate_first_next_process(ets:next(EtsTable, Key), EtsTable).

ets_to_list_match({_Size, EtsTable}) ->
	ets:match(EtsTable, '$1').

ets_to_list_select({_Size, EtsTable}) ->
	ets:select(EtsTable, [{'$1', [], ['$_']}]).

ets_insert({Size, EtsTable}) ->
	ets:insert(EtsTable, {list_to_binary(integer_to_list(random:uniform(Size))), <<"DUMMY VALUE">>}).

ets_delete({Size, EtsTable}) ->
	ets:delete(EtsTable, list_to_binary(integer_to_list(random:uniform(Size)))).

%% --------------------------------------------------------------------------------------------------------------------

proplist_get_value({Size, Proplist}) ->
	proplists:get_value(list_to_binary(integer_to_list(random:uniform(Size))), Proplist).

proplist_lookup({Size, Proplist}) ->
	proplists:lookup(list_to_binary(integer_to_list(random:uniform(Size))), Proplist).

proplist_iterate_list_comp({_Size, Proplist}) ->
	[
		null(Value)
		|| {_Key, Value} <- Proplist
	].

proplist_iterate_map({_Size, Proplist}) ->
	lists:map(fun({_Key, Value}) -> null(Value) end, Proplist).

proplist_iterate_foldl({_Size, Proplist}) ->
	lists:foldl(fun({_Key, Value}, _) -> null(Value) end, ok, Proplist).

proplist_keystore({Size, Proplist}) ->
	Key = list_to_binary(integer_to_list(random:uniform(Size))),
	lists:keystore(Key, 1, Proplist, {Key, <<"DUMMY VALUE">>}).

proplist_delete({Size, Proplist}) ->
	proplists:delete(list_to_binary(integer_to_list(random:uniform(Size))), Proplist).

%% --------------------------------------------------------------------------------------------------------------------

run_benches(Name, BenchFuns, Iterations, Repetitions) ->
	io:format("~n~s:~n", [Name]),
    io:format("                      Operation  Total (us)  Average (us)~n"),
	run_benches(BenchFuns, Iterations, Repetitions).

run_benches([], _Iterations, _Repetitions) ->
	[];

run_benches([{Name, Initial, TargetFun} | Rest], Iterations, Repetitions) ->
	run_benches([{Name, Initial, TargetFun, undefined} | Rest], Iterations, Repetitions);

run_benches([{Name, Initial, TargetFun, TeardownFun} | Rest], Iterations, Repetitions) ->
	gen_bench:setup(Initial),
	gen_bench:teardown(TeardownFun),

	RepetitionResults = gen_bench:benchmark_repeat(TargetFun, Iterations, Repetitions),

	LowestTotal = lists:min(lists:map(fun lists:sum/1, RepetitionResults)),
    io:format("  ~28s    ~8B    ~.6f~n", [Name, LowestTotal, LowestTotal / Iterations]),

	[LowestTotal | run_benches(Rest, Iterations, Repetitions)].
