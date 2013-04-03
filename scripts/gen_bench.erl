%%% @doc Benchmark shit!
%%% -------------------------------------------------------------------------------------------------------------------

-module(gen_bench).
-behavior(gen_server).

% API
-export([start_link/0, stop/0, setup/2, teardown/2, benchmark/3, benchmark_repeat/4]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------------------------------------------------------

stop() ->
	gen_server:call(?MODULE, stop).

%% --------------------------------------------------------------------------------------------------------------------

setup(Fun, Args) ->
	gen_server:call(?MODULE, {call_fun, Fun, Args}).

%% --------------------------------------------------------------------------------------------------------------------

teardown(Fun, Args) ->
	gen_server:call(?MODULE, {call_fun, Fun, Args}).

%% --------------------------------------------------------------------------------------------------------------------

benchmark(Fun, Args, Iterations) ->
	gen_server:call(?MODULE, {bench, Fun, Args, Iterations}, infinity).

%% --------------------------------------------------------------------------------------------------------------------

benchmark_repeat(Fun, Args, Iterations, Repetitions) ->
	[benchmark(Fun, Args, Iterations) || _ <- lists:seq(1, Repetitions)].

%% --------------------------------------------------------------------------------------------------------------------
%% gen_server
%% --------------------------------------------------------------------------------------------------------------------

init([]) ->
	{ok, state}.

%% --------------------------------------------------------------------------------------------------------------------

handle_call({call_fun, Fun, Args}, _From, state) ->
	Result = apply(Fun, Args),
	{reply, Result, state};

handle_call({bench, Fun, Args, Iterations}, _From, state) ->
	Times = [
		begin
			{Time, _Value} = timer:tc(Fun, Args),
			Time
		end
		|| _ <- lists:seq(1, Iterations)
	],
	{reply, Times, state};

handle_call(stop, _From, state) ->
	{stop, normal, ok, state};

handle_call(_Request, _From, state) ->
	{reply, {error, "WTFMFWTFAYT?"}, state}.

%% --------------------------------------------------------------------------------------------------------------------

handle_cast(_Request, state) ->
	{noreply, state}.

%% --------------------------------------------------------------------------------------------------------------------

handle_info(_Info, state) ->
	{noreply, state}.

%% --------------------------------------------------------------------------------------------------------------------

terminate(_Reason, state) ->
	whatever.

code_change(_OldVsn, state, _Extra) ->
	{error, "You can't tell me what to do."}.
