%%% @doc pre_ship_controller
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_ship_controller).

-behaviour(pre_gen_entity).

% API
-export([init/1, handle_info/4, removed/2, stopping/1]).

-record(pre_ship_state, {
	ship :: any()
}).

%% ---------------------------------------------------------------------------------------------------------------------
%% pre_gen_entity
%% ---------------------------------------------------------------------------------------------------------------------

init([Ship]) ->
	State = { ship = Ship },
	{ok, State}.

handle_info(Name, From, To, State) ->
	erlang:error(not_implemented).

removed(Why, State) ->
	erlang:error(not_implemented).

stopping(State) ->
	erlang:error(not_implemented).

%% ---------------------------------------------------------------------------------------------------------------------
