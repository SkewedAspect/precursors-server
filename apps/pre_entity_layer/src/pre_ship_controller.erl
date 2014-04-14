%%% @doc This is a base controller for all ships in the game.
%%%
%%% It is not intended to be tied to a player; NPC ships should also be able to use this controller as a base.
%%%
%%% The intention is for this to implement all of the low-level details of a ship, like movement (and associated input
%%% events), etc. More specific modules (for each of the ship 'classes') will simply handle the specific events they
%% care about, and then use this module as their fallback case. (Alternatively, they can call this module directly.)
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_ship_controller).

-behaviour(pre_gen_entity).

-record(state, {
	ship :: any()
}).

% API
-export([init/1, handle_event/5, removed/2, stopping/1, simulate/2]).

%% ---------------------------------------------------------------------------------------------------------------------
%% pre_gen_entity
%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Called when the entity is added the the entity engine.
init([Ship]) ->
	State = #state{ ship = Ship },
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Handles the remove event
handle_event(remove, _From, _To, Reason, State) ->
	Ship = State#state.ship,
	lager:debug("Removing ship \"~p\" for reason: \"~p\".", [Ship:id(), Reason]),
	remove_entity;

%% @doc Handles incoming events
handle_event(Name, From, To, Data, State) ->
	lager:warning("Unhandled event: ~p ~p ~p ~p", [Name, From, To, Data]),
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc Called when the ship is removed from the entity engine.
removed(Why, State) ->
	Ship = State#state.ship,
	lager:debug("Removed ship \"~p\"; reason: \"~p\"", [Ship:id(), Why]),

	% Save the latest state of the ship
	Ship = State#state.ship,
	Ship:save(),

	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

%% @doc ???
stopping(State) ->
	Ship = State#state.ship,
	lager:debug("Stopping ship ~p", [Ship:id()]),
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

simulate(Delta, State) ->
	%TODO: Simulate movement!
	{ok, State}.

%% ---------------------------------------------------------------------------------------------------------------------

