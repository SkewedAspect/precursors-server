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

% State record
-record(state, {
	entities = [] :: list(),
	updates = dict:new() :: dict()
}).

% Simulation interval
-define(INTERVAL, 33). % about 1/60th of a second. (16 ms)

%% --------------------------------------------------------------------------------------------------------------------
%% Worker API
%% --------------------------------------------------------------------------------------------------------------------

start_worker(InitialEntities) ->
	spawn_link(?MODULE, init, [InitialEntities]).

%% --------------------------------------------------------------------------------------------------------------------

init(InitialEntities) ->
	InitialState = #state {
		entities = InitialEntities
	},

	% Start the simulate timer
	erlang:send_after(?INTERVAL, self(), simulate),

	% Start the receive loop.
	do_receive(InitialState).

%% --------------------------------------------------------------------------------------------------------------------

do_receive(State) ->
	% Listen for messages being sent to the process.
	{Reply, NewState, From} = receive
		{From1, Msg} ->
			{Reply1, NewState1} = handle_msg(Msg, From1, State),
			{Reply1, NewState1, From1}
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
	% Would do simulate here.

	%TODO: Need the code for detecting how long simulate too, and adjusting interval that much.

	% Restart the simulate timer
	erlang:send_after(?INTERVAL, self(), simulate),

	{noreply, State};

handle_msg(_Msg, _From, State) ->
	{noreply, State}.
