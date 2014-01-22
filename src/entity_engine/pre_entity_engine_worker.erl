%%% @doc The entity engine's simulation worker process.
%%%
%%% This module has exactly one purpose: it runs entity simulations. That is all that it does. It is written as a pure
%%% Erlang module, not an OTP module, because of issues encountered with running gen_server-based modules at a fixed
%%% rate. There is an exact one-to-one relationship between entity engine workers and entity engines. The worker also
%%% has its own copy of the entity engine's state. This is stored as a proplist rather than as a dict, for performance
%%% reasons.
%%%
%%% The entire reason for this extra process is to move the load of physics simulation to a different process from the
%%% one that has to reply to updates, or communicate with the rest of the system. This process can run at full speed,
%%% only responding to, or generating updates. The rest is handled by the entity engine proper.
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_engine_worker).

-include("log.hrl").
-include("pre_entity.hrl").

% API
-export([start_worker/1, init/1]).

-ifdef(TEST).
-export([simulate_entities/1]).
-endif.

% State record
-record(state, {
	entities = [] :: list(),
	incoming_updates = dict:new() :: dict(),
	last_simulate :: integer()
}).

% Simulation interval
-define(INTERVAL, 33). % about 1/60th of a second. (16 ms)

% Simulation time at which we start warning about our load.
-define(WARN_INTERVAL, (?INTERVAL - (?INTERVAL * 0.05))). % 5% of Interval

%% --------------------------------------------------------------------------------------------------------------------
%% Worker API
%% --------------------------------------------------------------------------------------------------------------------

start_worker(InitialEntities) ->
	spawn_link(?MODULE, init, [InitialEntities]).

%% --------------------------------------------------------------------------------------------------------------------

init(InitialEntities) ->
	?info("pre_entity_engine_worker:init(~p)", [InitialEntities]),

	% Set our priority to high.
	process_flag(priority, high),

	InitialState = #state {
		entities = InitialEntities
	},

	% Join the entity_updates process group. (created by the entity engine process if needed)
	pg2:join(entity_updates, self()),

	% Start the simulation timer
	erlang:send_after(?INTERVAL, self(), {self(), simulate}),

	% Start the receive loop.
	do_receive(InitialState).

%% --------------------------------------------------------------------------------------------------------------------

do_receive(State) ->
	% Listen for messages being sent to the process.
	{Reply, NewState, From} = receive
		{From1, Msg} when is_pid(From1) ->
			{Reply1, NewState1} = handle_msg(Msg, From1, State),
			{Reply1, NewState1, From1};
		Other ->
			?error("pre_entity_engine_worker:do_receive: Received invalid message: ~p", [Other]),
			{noreply, State, undefined}
	end,

	% Handle potential replies
	case Reply of
		noreply ->
			ok;
		_ ->
			From ! Reply
	end,

	% Time to call do_simulate again.
	do_receive(NewState).

%% --------------------------------------------------------------------------------------------------------------------

handle_msg(simulate, _From, State) ->
	LastSim = State#state.last_simulate,
	Start = erlang:now(),
	SimGap = if
		LastSim == undefined ->
			0;
		true ->
			(timer:now_diff(Start, LastSim) / 1000)
	end,

	if
		SimGap >= (?INTERVAL + (?INTERVAL * 0.25)) ->
			?error("Extremely long time between simulate calls: Entity Engine: ~p, time: ~p", [self(), SimGap]);

		SimGap >= (?INTERVAL + (?INTERVAL * 0.1)) ->
			?warn("Overly long time between simulate calls: Entity Engine: ~p, time: ~p", [self(), SimGap]);

		true -> ok
	end,

	% Simulate all our entities
	State1 = simulate_entities(State),

	% Record the amount of time taken to perform simulate
	SimTime = (timer:now_diff(erlang:now(), Start) / 1000),

	% Report if there's issues.
	NextInterval = if
		SimTime >= ?INTERVAL ->
			?error("Overloaded Entity Engine ~p (~p ms).", [self(), SimTime]),
			0;
		SimTime >= ?WARN_INTERVAL ->
			?warn("High Load on Entity Engine ~p (~p ms).", [self(), SimTime]),
			round(?INTERVAL - SimTime);
		true ->
			round(?INTERVAL - SimTime)
	end,

	% Restart the simulation timer
	erlang:send_after(NextInterval, self(), {self(), simulate}),

	State2 = State1#state {
		last_simulate = Start
	},

    {noreply, State2};

handle_msg({add_entity, NewEntity}, _From, State) ->
	?info("pre_entity_engine_worker[~p]: adding entity ~p", [self(), NewEntity]),
	Entities = State#state.entities,
	State1 = State#state {
		entities = [NewEntity | Entities]
	},
	{noreply, State1};

handle_msg({updates, NewUpdates}, _From, State) ->
	#state {
		incoming_updates = IncomingUpdates
	} = State,

	NewIncomingUpdates = lists:foldl(
		fun({EntityID, Update}, IncomingUpdates1) ->
			dict:append(EntityID, Update, IncomingUpdates1)
		end,
		IncomingUpdates,
		NewUpdates
	),

	State1 = State#state {
		incoming_updates = NewIncomingUpdates
	},
	{noreply, State1};

handle_msg(Msg, From, State) ->
	?warn("pre_entity_engine_worker:handle_msg(~p, ~p, ~p): Unrecognized message!", [Msg, From, State]),
	{noreply, State}.

%% --------------------------------------------------------------------------------------------------------------------
%% Internal API
%% --------------------------------------------------------------------------------------------------------------------

simulate_entities(State) ->
	#state {
		entities = Entities,
		incoming_updates = Updates
	} = State,

	{NewEntities2, OutgoingUpdates2} = lists:foldl(
		fun(Entity, {NewEntities1, OutgoingUpdates1}) ->
			IncomingEntityUpdates = dict:find(Entity#entity.id, Updates),

			% Wrap return_result, passing the new entities and outgoing updates lists.
			ReturnResult = fun(ControllerFuncResult, Context) ->
				return_result(ControllerFuncResult, Context, {NewEntities1, OutgoingUpdates1})
			end,

			% First, apply any incoming updates.
			Entity1 = case IncomingEntityUpdates of
				error -> Entity;
				{ok, []} -> Entity;
				{ok, EntityUpdates} -> entity_controller:apply_updates(lists:flatten(EntityUpdates), Entity)
			end,

			% Then, call simulate.
			entity_controller:call(Entity1, simulate, [Entity1, State], ReturnResult)
		end,
		{[], []},
		Entities
	),

	pre_entity_engine_sup:broadcast_updates(OutgoingUpdates2),

	State#state {
		entities = NewEntities2,
		incoming_updates = dict:new()
	}.

-spec return_result(ControllerFuncResult, Context, SimulateEntityResponse) -> SimulateEntityResponse when
	ControllerFuncResult :: term(),
	Context :: {OriginalEntity, Func, Args},
		OriginalEntity :: #entity{},
		Func :: atom(),
		Args :: list(),
	SimulateEntityResponse :: {NewEntities, OutgoingUpdates},
		NewEntities :: [#entity{}],
		OutgoingUpdates :: [json()].

% 2-tuples
return_result({undefined, #entity{} = NewEntity}, _Ctx, {NewEntities, OutgoingUpdates}) ->
	{[NewEntity | NewEntities], OutgoingUpdates};

return_result({Update, #entity{} = NewEntity}, _Ctx, {NewEntities, OutgoingUpdates}) ->
	EntityID = NewEntity#entity.id,
	{[NewEntity | NewEntities], [{EntityID, Update} | OutgoingUpdates]};

return_result({_, UnrecognizedEnt}, {OriginalEntity, Func, Args}, {NewEntities, OutgoingUpdates}) ->
	Controller = OriginalEntity#entity.controller,
	?error("Unrecognized entity record in 2-tuple result from ~p:~p(~p): ~p", [Controller, Func, Args, UnrecognizedEnt]),
	{[OriginalEntity | NewEntities], OutgoingUpdates};

% Other
return_result(Unrecognized, {OriginalEntity, Func, Args}, {NewEntities, OutgoingUpdates}) ->
	Controller = OriginalEntity#entity.controller,
	?error("Unrecognized result from ~p:~p(~p): ~p", [Controller, Func, Args, Unrecognized]),
	{[OriginalEntity | NewEntities], OutgoingUpdates}.
