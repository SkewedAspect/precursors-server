%% @doc Periodically calls the {@link pre_gen_entity:run_sumulations/1}
%% for each gen_event registered to pre_ge_sup. The interval is taken from
%% {@link pre_gen_entity:simulate_interval/0}.
-module(pre_entity_sim_timer).

-behavior(gen_server).

% api
-export([start_link/0]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	interval, timer
}).

% api

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server callbacks.

init(_) ->
	Interval = pre_gen_entity:simulate_interval(),
	Timer = create_timer(Interval),
	State = #state{timer = Timer, interval = Interval},
	{ok, State}.

handle_call(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(run_simulations, State) ->
	_ = cancel_timer(State#state.timer),
	GenEvents = pre_ge_sup:running_children(),
	_ = lists:foreach(fun run_sim/1, GenEvents),
	Timer = create_timer(State#state.interval),
	State2 = State#state{timer = Timer},
	{noreply, State2}.

terminate(_Why, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

cancel_timer(Timer) ->
	_ = erlang:cancel_timer(Timer),
	consume_sim_msgs().

consume_sim_msgs() ->
	receive
		run_simulations ->
			consume_sim_msgs()
	after 0 ->
		ok
	end.

create_timer(Interval) ->
	Self = self(),
	erlang:send_after(Interval, Self, run_simulations).

run_sim(GenEvent) ->
	pre_gen_entity:run_simulations(GenEvent).

