%%% @doc Callbacks for the "entity" channel.

-module(pre_chat_channel).

-behaviour(pre_gen_channel).

-include("pre_client.hrl").

% API
-export([handle_request/4, handle_response/4, handle_event/3]).

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_request(Type, ID, Request, State) ->
	lager:warning("[Chat] Unknown Request: ~p, ~p, ~p", [Type, ID, Request]),
	%TODO: Forward to the chat system.
	State.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_response(Type, ID, Request, State) ->
	lager:warning("[Chat] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	%TODO: Forward to the chat system.
	State.

%% ---------------------------------------------------------------------------------------------------------------------

%% @private
handle_event(Type, Request, State) ->
	lager:warning("[Chat] Unknown Event: ~p, ~p", [Type, Request]),
	%TODO: Forward to the chat system.
	State.

