#!/usr/bin/env escript
%%! -noshell -noinput -smp enable
%%! -pa ../ebin

%% Benchmarking key/value data types
%%
%% Running:
%%
%%   $ erlc -smp gen_bench.erl
%%   $ escript run_bench.escript

-mode(compile).

-compile(export_all).

-record(entity, {
	id :: binary(),
	client :: {client_info},
	behavior :: atom(),
	model = [{model, <<"Ships/ares">>}] :: json(),
	state :: dict()
}).

-define(ITERS, 1000).
-define(REPS, 20).

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
op_color(to_list_select) -> ?ATC_TO_LIST;

op_color(_) -> ?ATC_DEFAULT.

%% --------------------------------------------------------------------------------------------------------------------

main([]) ->
	gen_bench:start_link(),

	code:add_path("../ebin"),

	io:format("Running benchmarks with ~s~p~s repetitions of ~s~p~s iterations each.~n~n",
		[atc(bold), ?REPS, atc(reset), atc(bold), ?ITERS, atc(reset)]),

	io:format("The statistics below only include data from the ~sfastest repetition~s of each benchmark;~n",
		[atc(bold), atc(reset)]),
	io:format("all other repetitions are discarded.~n"),

	baseline(),

	Tests = [
		physics
	],
	Sizes = [
		1
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

physics(_Size) ->
	SetupFunc = fun() ->
		Entity = #entity{
			id = <<"1">>,
			behavior = entity_physical,
			state = dict:new()
		},
		entity_physical:init(Entity)
	end,
	run_benches(
		"Physical Behavior",
		[
			{physical_defaults_simulate, SetupFunc, fun(Foo) -> phsyical_defaults(Foo) end}
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

phsyical_defaults(Entity) ->
	entity_physical:simulate(Entity, undef).

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
