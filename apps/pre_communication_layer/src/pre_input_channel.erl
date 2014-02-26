%%% @doc Callbacks for the "entity" channel.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_input_channel).

-behaviour(pre_gen_channel).

-include("pre_client.hrl").

% API
-export([handle_request/4, handle_response/4, handle_event/4]).

%% ---------------------------------------------------------------------------------------------------------------------

handle_request(Type, ID, Request, State) ->
	%TODO: forward requests on to the entity event engine.
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_response(Type, ID, Request, State) ->
	lager:warning("[Input] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_event(Type, ID, Request, State) ->
	%TODO: forward events on to the entity event engine.
	State.