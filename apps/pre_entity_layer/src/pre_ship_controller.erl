%%% @doc This is a base controller for all ships in the game.
%%%
%%% It is not intended to be tied to a player; NPC ships should also be
%%% able to use this controller as a base.
%%%
%%% The intention is for this to implement all of the low-level details of a ship, like movement (and associated input
%%% events), etc. More specific modules (for each of the ship 'classes') will simply handle the specific events they
%%% care about, and then use this module as their fallback case. (Alternatively, they can call this module directly.)
%%%
%%% Currently the only event handled is remove. Does not fire any events.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_ship_controller).

-behaviour(pre_gen_entity).

-record(state, {
	ship :: any(),
	physical
}).

% API
-export([init/1, handle_event/5, removed/2, stopping/1, simulate/2]).

%% ---------------------------------------------------------------------------------------------------------------------
%% pre_gen_entity
%% ---------------------------------------------------------------------------------------------------------------------

%% @private
init([Ship]) ->
	State = #state{ ship = Ship },
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_event(remove, _From, _To, Reason, State) ->
	Ship = State#state.ship,
	lager:debug("Removing ship ~p for reason: ~p", [Ship:id(), Reason]),
	remove_entity;

%% @doc Handles incoming events
handle_event(Name, From, To, Data, State) ->
	lager:warning("Unhandled event: ~p ~p ~p ~p", [Name, From, To, Data]),
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
removed(Why, State) ->
	Ship = State#state.ship,
	lager:debug("Removed ship ~p; reason: ~p", [Ship:id(), Why]),

	% Save the latest state of the ship
	Ship = State#state.ship,
	Ship:save(),

	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
stopping(State) ->
	Ship = State#state.ship,
	lager:debug("Stopping ship ~p", [Ship:id()]),
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
-spec simulate(TimestepUsec, State) -> UpdatedState when
	TimestepUsec :: float(),
	State :: #state{},
	UpdatedState :: #state{}.

simulate(TimestepUsec, State) ->
	%TODO: Simulate movement!
	Physical = State#state.physical,
	UpdatedPhysical = Physical:simulate(TimestepUsec),
	{ok, State#state{ physical = UpdatedPhysical }}.

%% ---------------------------------------------------------------------------------------------------------------------
