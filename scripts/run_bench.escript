#!/usr/bin/env escript
%%! -noshell -noinput -smp enable -pa ebin

%% Benchmarking key/value data types
%%
%% Running:
%%
%%   $ erlc -smp gen_bench.erl
%%   $ escript run_bench.escript

-mode(compile).

-compile(export_all).

-define(ITERS, 100).
-define(REPS, 10).

%% --------------------------------------------------------------------------------------------------------------------
%% ANSI Terminal Colors!

% Control Sequence Introducer (ESC + '[')
-define(CSI, 27, $[).

atc(A1, A2, A3) -> <<?CSI, (atc_elem(A1))/binary, $;, (atc_elem(A2))/binary, $;, (atc_elem(A3))/binary, $m>>.
atc(A1, A2) -> <<?CSI, (atc_elem(A1))/binary, $;, (atc_elem(A2))/binary, $m>>.
atc(A1) -> <<?CSI, (atc_elem(A1))/binary, $m>>.

atc_elem(reset) -> <<$0>>;
atc_elem(bold) -> <<$1>>;
atc_elem(underline) -> <<$4>>;
atc_elem(blink) -> <<$5>>;
atc_elem(Bin) when is_binary(Bin) -> Bin;
atc_elem(Color) when is_atom(Color) orelse is_integer(Color) -> fg(Color);
atc_elem(ElemList) when is_list(ElemList) -> atc_elem_list(ElemList).

atc_elem_list([]) -> <<>>;
atc_elem_list([Head | Rest]) -> <<(atc_elem(Head))/binary, $;, (atc_elem(Rest))/binary>>.

% ANSI terminal colors
atc_color(black) -> $0;
atc_color(red) -> $1;
atc_color(green) -> $2;
atc_color(yellow) -> $3;
atc_color(blue) -> $4;
atc_color(magenta) -> $5;
atc_color(cyan) -> $6;
atc_color(white) -> $7.

% Foreground colors
fg(Color) when is_atom(Color) -> <<$3, (atc_color(Color))>>;
fg(Num) when is_integer(Num) -> <<"38;5;", (list_to_binary(integer_to_list(Num)))/binary>>.
bright_fg(Color) when is_atom(Color) -> <<$9, (atc_color(Color))>>.

% Background colors
bg(Color) when is_atom(Color) -> <<$4, (atc_color(Color))>>;
bg(Num) when is_integer(Num) -> <<"48;5;", (list_to_binary(integer_to_list(Num)))/binary>>.
bright_bg(Color) when is_atom(Color) -> <<$1, $0, (atc_color(Color))>>.

%% --------------------------------------------------------------------------------------------------------------------
%% Colors for different categories of operations

-define(ATC_TITLE, atc(bold, cyan)).
-define(ATC_COLUMN, atc(underline, 251)).
-define(ATC_CREATING, atc(yellow)).
-define(ATC_GETTING, atc(bold, green)).
-define(ATC_SETTING, atc(bold, 81)).
-define(ATC_DELETING, atc(bold, red)).
-define(ATC_ITERATING, atc(202)).
-define(ATC_TO_LIST, atc(180)).
-define(ATC_DEFAULT, atc(243)).

% Creating
op_color(create) -> ?ATC_CREATING;
op_color(initialize) -> ?ATC_CREATING;
op_color(new) -> ?ATC_CREATING;
op_color(create_new_store) -> ?ATC_CREATING;
op_color(create_from_list) -> ?ATC_CREATING;

% Getting
op_color(fetch) -> ?ATC_GETTING;
op_color(get) -> ?ATC_GETTING;
op_color(get_value) -> ?ATC_GETTING;
op_color(lookup) -> ?ATC_GETTING;

% Setting
op_color(insert) -> ?ATC_SETTING;
op_color(keystore) -> ?ATC_SETTING;
op_color(put) -> ?ATC_SETTING;
op_color(store) -> ?ATC_SETTING;

% Deleting
op_color(delete) -> ?ATC_DELETING;
op_color(erase) -> ?ATC_DELETING;

% Iterating
op_color(iterate_first_next) -> ?ATC_ITERATING;
op_color(iterate_fold) -> ?ATC_ITERATING;
op_color(iterate_foldl) -> ?ATC_ITERATING;
op_color(iterate_fold_map) -> ?ATC_ITERATING;
op_color(iterate_list_comp) -> ?ATC_ITERATING;
op_color(iterate_map) -> ?ATC_ITERATING;
op_color(iterate_select) -> ?ATC_ITERATING;

% Converting to list
op_color(to_list) -> ?ATC_TO_LIST;
op_color(to_list_fold) -> ?ATC_TO_LIST;
op_color(to_list_list_comp) -> ?ATC_TO_LIST;
op_color(to_list_match) -> ?ATC_TO_LIST;
op_color(to_list_select) -> ?ATC_TO_LIST;

op_color(_) -> ?ATC_DEFAULT.

%% --------------------------------------------------------------------------------------------------------------------

main([]) ->
	gen_bench:start_link(),

	io:format("Running benchmarks with ~s~p~s repetitions of ~s~p~s iterations each.~n~n",
		[atc(bold), ?REPS, atc(reset), atc(bold), ?ITERS, atc(reset)]),

	io:format("The statistics below only include data from the ~sfastest repetition~s of each benchmark;~n",
		[atc(bold), atc(reset)]),
	io:format("all other repetitions are discarded.~n"),

	baseline(),

	Tests = [
		%pdict,
		%dict,
		ets%,
		%proplist
	],
	Sizes = [
		1,
		10,
		100,
		1000,
		10000,
		100000
	],
	[
		?MODULE:Test(Size)
		|| Size <- Sizes, Test <- Tests
	].

%% --------------------------------------------------------------------------------------------------------------------

baseline() ->
	run_benches(
		"Baseline",
		[
			{basic_operation, fun ?MODULE:null/0, fun ?MODULE:null/1},
			{operation_with_generated_key, fun ?MODULE:null/0, fun ?MODULE:null_gen_key/1}
		],
		?ITERS, ?REPS
	).

dict(Size) ->
	SetupFunc = fun() -> dict_create_from_list(Size) end,
	run_benches(
		io_lib:format("dict (~p items)", [Size]),
		[
			{create_new_store, fun() -> Size end, fun ?MODULE:dict_create_new_store/1},
			{create_from_list, fun() -> Size end, fun ?MODULE:dict_create_from_list/1},
			{fetch, SetupFunc, fun ?MODULE:dict_fetch/1},
			{iterate_map, SetupFunc, fun ?MODULE:dict_iterate_map/1},
			{iterate_fold, SetupFunc, fun ?MODULE:dict_iterate_fold/1},
			{iterate_fold_map, SetupFunc, fun ?MODULE:dict_iterate_fold_map/1},
			{to_list, SetupFunc, fun ?MODULE:dict_to_list/1},
			{to_list_fold, SetupFunc, fun ?MODULE:dict_to_list_fold/1},
			{store, SetupFunc, fun ?MODULE:dict_store/1},
			{erase, SetupFunc, fun ?MODULE:dict_erase/1}
		],
		?ITERS, ?REPS
	).

pdict(Size) ->
	SetupFunc = fun() -> pdict_initialize(Size) end,
	run_benches(
		io_lib:format("Process dictionary (~p items)", [Size]),
		[
			{initialize, fun() -> Size end, fun ?MODULE:pdict_initialize/1},
			{get, SetupFunc, fun ?MODULE:pdict_get/1},
			{iterate_list_comp, SetupFunc, fun ?MODULE:pdict_iterate_list_comp/1},
			{iterate_map, SetupFunc, fun ?MODULE:pdict_iterate_map/1},
			{iterate_foldl, SetupFunc, fun ?MODULE:pdict_iterate_foldl/1},
			{to_list_list_comp, SetupFunc, fun ?MODULE:pdict_to_list_list_comp/1},
			{put, SetupFunc, fun ?MODULE:pdict_put/1},
			{erase, SetupFunc, fun ?MODULE:pdict_erase/1}
		],
		?ITERS, ?REPS
	).

ets(Size) ->
	SetupFunc = fun() -> ets_new(Size) end,
	run_benches(
		io_lib:format("ets (~p items)", [Size]),
		[
			{new, fun() -> Size end, fun ?MODULE:ets_new/1},
			{lookup, SetupFunc, fun ?MODULE:ets_lookup/1},
			{iterate_lookup, SetupFunc, fun ?MODULE:ets_iterate_lookup/1}, % HORRIBLE?
			{iterate_foldl, SetupFunc, fun ?MODULE:ets_iterate_foldl/1}, % HORRIBLE
			{iterate_match, SetupFunc, fun ?MODULE:ets_iterate_match/1}, % Not as bad, but still BAD
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
	SetupFunc = fun() -> proplist_create(Size) end,
	run_benches(
		io_lib:format("proplist (~p items)", [Size]),
		[
			{create, fun() -> Size end, fun ?MODULE:proplist_create/1},
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

dict_create_new_store(Size) ->
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
	}.

dict_create_from_list(Size) ->
	{Size,
		dict:from_list([
			{list_to_binary(integer_to_list(Key)), <<"Value ", Key/integer>>}
			|| Key <- lists:seq(1, Size)
		])
	}.

dict_fetch({Size, Dict}) ->
	dict:fetch(list_to_binary(integer_to_list(random:uniform(Size))), Dict).

dict_iterate_map({_Size, Dict}) ->
	dict:map(fun(_Key, Value) -> null(Value) end, Dict).

dict_iterate_fold({_Size, Dict}) ->
	dict:fold(fun(_Key, Value, _) -> null(Value) end, ok, Dict).

dict_iterate_fold_map({_Size, Dict}) ->
	dict:fold(fun(Key, Value, NewDict) -> dict:store(Key, Value, NewDict) end, dict:new(), Dict).

dict_to_list({_Size, Dict}) ->
	dict:to_list(Dict).

dict_to_list_fold({_Size, Dict}) ->
	dict:fold(fun(Key, Value, Acc) -> [{Key, Value} | Acc] end, ok, Dict).

dict_store({Size, Dict}) ->
	dict:store(list_to_binary(integer_to_list(random:uniform(Size))), <<"DUMMY VALUE">>, Dict).

dict_erase({Size, Dict}) ->
	dict:erase(list_to_binary(integer_to_list(random:uniform(Size))), Dict).

%% --------------------------------------------------------------------------------------------------------------------

pdict_initialize(Size) ->
	[
		put({ourshit, list_to_binary(integer_to_list(Key))}, <<"Value ", Key/integer>>)
		|| Key <- lists:seq(1, Size)
	],
	{Size}.

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

pdict_put({Size}) ->
	put({ourshit, list_to_binary(integer_to_list(random:uniform(Size)))}, <<"DUMMY VALUE">>).

pdict_erase({Size}) ->
	erase({ourshit, list_to_binary(integer_to_list(random:uniform(Size)))}).

%% --------------------------------------------------------------------------------------------------------------------

ets_new(Size) ->
	EtsTable = ets:new(whatthefuck, [private]),
	[
		ets:insert(EtsTable, {list_to_binary(integer_to_list(Key)), <<"Value ", Key/integer>>})
		|| Key <- lists:seq(1, Size)
	],
	{Size, EtsTable}.

ets_lookup({Size, EtsTable}) ->
	ets:lookup(EtsTable, list_to_binary(integer_to_list(random:uniform(Size)))).

ets_iterate_lookup({Size, EtsTable}) ->
	[ets:lookup(EtsTable, list_to_binary(integer_to_list(Key))) || Key <- lists:seq(1, Size)].

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

proplist_create(Size) ->
	Proplist = [
		{list_to_binary(integer_to_list(Key)), <<"Value ", Key/integer>>}
		|| Key <- lists:seq(1, Size)
	],
	{Size, Proplist}.

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
	io:format("~n~s~s:~s~n", [?ATC_TITLE, Name, atc(reset)]),
	io:format(
		"                      ~sOperation~s  ~sTotal (us)~s  ~sMinimum (us)~s  ~sAverage (us)~s"
			++ "  ~sMaximum (us)~s  ~sStd. Dev. (us)~s  ~sAvg. Iter./Sec.~s~n",
		[?ATC_COLUMN, atc(reset), ?ATC_COLUMN, atc(reset), ?ATC_COLUMN, atc(reset), ?ATC_COLUMN, atc(reset),
			?ATC_COLUMN, atc(reset), ?ATC_COLUMN, atc(reset), ?ATC_COLUMN, atc(reset)]),
	run_benches(BenchFuns, Iterations, Repetitions).

run_benches([], _Iterations, _Repetitions) ->
	[];

run_benches([{Name, Initial, TargetFun} | Rest], Iterations, Repetitions) ->
	run_benches([{Name, Initial, TargetFun, undefined} | Rest], Iterations, Repetitions);

run_benches([{Name, Initial, TargetFun, TeardownFun} | Rest], Iterations, Repetitions) ->
	gen_bench:setup(Initial),
	gen_bench:teardown(TeardownFun),

	RepetitionResults = gen_bench:benchmark_repeat(TargetFun, Iterations, Repetitions),

	Lowest = lists:foldl(
		fun(Results, {PrevLowestTotal, _} = PrevLowest) ->
			TotalTime = lists:sum(Results),
			if
				TotalTime < PrevLowestTotal -> {TotalTime, Results};
				true -> PrevLowest
			end
		end,
		{infinity, []},
		RepetitionResults
	),

	display_results(Name, Lowest, Iterations),

	[Lowest | run_benches(Rest, Iterations, Repetitions)].

%% --------------------------------------------------------------------------------------------------------------------

display_results(Name, {TotalTime, Results}, Iterations) ->
	Minimum = float(lists:min(Results)),
	Average = float(TotalTime / Iterations),
	Maximum = float(lists:max(Results)),
	StdDev = float(std_dev(Results, Iterations)),
	AvgIterPerSec = 1000000 / Average, % = 1 / AvgSec, where AvgSec = Average / 1000000 (since Average is microseconds)

    io:format("  ~s~28w    ~8B    ~-10.3f    ~-10.3f    ~-10.3f    ~-11.3f    ~-13.3f~s~n",
		[op_color(Name), Name, TotalTime, Minimum, Average, Maximum, StdDev, AvgIterPerSec, atc(reset)]
	).

%% --------------------------------------------------------------------------------------------------------------------

std_dev(Results, Iterations) ->
	Average = lists:sum(Results) / Iterations,
	F = fun(X, Sum) -> Sum + (X - Average) * (X - Average) end,
	Variance = lists:foldl(F, 0.0, Results) / Iterations,
	math:sqrt(Variance).

% vim: ft=erlang
