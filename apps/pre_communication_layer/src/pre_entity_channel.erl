%%% @doc Callbacks for the "entity" channel.

-module(pre_entity_channel).

-behaviour(pre_gen_channel).

-include("pre_client.hrl").

% API
-export([handle_request/4, handle_response/4, handle_event/3]).

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_request(<<"full">>, ID, Request, State) ->
	lager:warning("Full update requested."),
	case State#client_state.entity of
		undefined ->
			Response = [
				{confirm, false},
				{reason, <<"No entity inhabited">>}
			],
			pre_client:send_response(self(), <<"entity">>, ID, Response);
		_ ->
			EntityID = proplists:get_value(id, Request),
			%TODO: get the full state of the entity.
			Response = [
				{confirm, false},
				{reason, <<"Not implemented yet!">>}
			],
			pre_client:send_response(self(), <<"entity">>, ID, Response)
	end,
	State;


handle_request(Type, ID, Request, State) ->
	%TODO: Forward to the entity event engine.
	State.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_response(Type, ID, Request, State) ->
	lager:warning("[Entity] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_event(Type, Request, State) ->
	%TODO: Forward to the entity event engine.
	State.
