%%% @doc Benchmark shit!
%%% -------------------------------------------------------------------------------------------------------------------

-module(gen_bench).
-behavior(gen_server).

% API
-export([start_link/0, stop/0]).
-export([setup/1, setup/2, teardown/1, teardown/2]).
-export([benchmark/2, benchmark/3, benchmark_repeat/3, benchmark_repeat/4]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	setup :: function(),
	teardown :: function()
}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------

stop() ->
	gen_server:call(?MODULE, stop).

%% --------------------------------------------------------------------------------------------------------------------

setup(undefined) ->
	ok;
setup(Fun) ->
	setup(Fun, []).

setup(Fun, Args) ->
	gen_server:call(?MODULE, {set_setup_fun, Fun, Args}).

%% --------------------------------------------------------------------------------------------------------------------

teardown(undefined) ->
	ok;
teardown(Fun) ->
	teardown(Fun, []).

teardown(Fun, Args) ->
	gen_server:call(?MODULE, {set_teardown_fun, Fun, Args}).

%% --------------------------------------------------------------------------------------------------------------------

benchmark(Fun, Iterations) ->
	benchmark(Fun, [], Iterations).

benchmark(Fun, Args, Iterations) ->
	gen_server:call(?MODULE, {bench, Fun, Args, Iterations}, infinity).

%% --------------------------------------------------------------------------------------------------------------------

benchmark_repeat(Fun, Iterations, Repetitions) ->
	benchmark_repeat(Fun, [], Iterations, Repetitions).

benchmark_repeat(Fun, Args, Iterations, Repetitions) ->
	gen_server:call(?MODULE, {bench_repeat, Fun, Args, Iterations, Repetitions}, infinity).

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([]) ->
	{ok, #state{}}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({set_setup_fun, Fun, Args}, _From, State) ->
	{reply,
		ok,
		State#state{
			setup = {Fun, Args}
		}
	};

handle_call({set_teardown_fun, Fun, Args}, _From, State) ->
	{reply,
		ok,
		State#state{
			teardown = {Fun, Args}
		}
	};

handle_call({bench, Fun, Args, Iterations}, _From, State) ->
	#state{
		setup = Setup,
		teardown = Teardown
	} = State,

	Times = [
		begin
			run_bench_proc(Setup, {Fun, Args}, Teardown)
		end
		|| _ <- lists:seq(1, Iterations)
	],
	{reply, Times, #state{}}; % Reset state so we don't reuse the old setup/teardown funcs.

handle_call({bench_repeat, Fun, Args, Iterations, Repetitions}, From, State) ->
	RepetitionTimes = [
		begin
			{reply, Times, _NewState} = handle_call({bench, Fun, Args, Iterations}, From, State),
			Times
		end
		|| _ <- lists:seq(1, Repetitions)
	],
	{reply, RepetitionTimes, #state{}}; % Reset state so we don't reuse the old setup/teardown funcs.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Request, _From, State) ->
	{reply, {error, "WTFMFWTFAYT?"}, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast(_Request, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(_Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(_Reason, _State) ->
	whatever.

code_change(_OldVsn, _State, _Extra) ->
	{error, "You can't tell me what to do."}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal Helpers
%% --------------------------------------------------------------------------------------------------------------------

run_bench_proc(Setup, Target, Teardown) ->
	GenBenchPid = self(),

	% Spawn a new benchmarking process to run the target function in a clean environment.
	spawn_link(fun() ->
		bench_proc(GenBenchPid, Setup, Target, Teardown)
	end),

	% Wait until we get the time back from the benchmarking process.
	receive
		{time, Time} -> Time
	end.

bench_proc(GenBenchPid, Setup, Target, Teardown) ->
	% Run the setup function if one was specified.
	InitialValue = case Setup of
		{SFun, SArgs} ->
			apply(SFun, SArgs);
		undefined ->
			undefined
	end,

	% Run and time the target function.
	{TFun, TArgs} = Target,
	{Time, _Value} = timer:tc(TFun, [InitialValue | TArgs]),

	% Run the teardown function if one was specified.
	case Teardown of
		{TDFun, TDArgs} ->
			apply(TDFun, [InitialValue | TDArgs]);
		undefined ->
			undefined
	end,

	% Send the recorded time back to gen_bench.
	GenBenchPid ! {time, Time},

	% Exit normally.
	normal.
