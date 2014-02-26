%%% @doc Callbacks for the "entity" channel.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_chat_channel).

-behaviour(pre_channel).

-include("pre_client.hrl").

% API
-export([handle_request/2, handle_response/2, handle_event/2]).

%% ---------------------------------------------------------------------------------------------------------------------

handle_request({Type, ID, Request}, State) ->
	lager:warning("[Chat] Unknown Request: ~p, ~p, ~p", [Type, ID, Request]),
	%TODO: Forward to the chat system.
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_response({Type, ID, Request}, State) ->
	lager:warning("[Chat] Unknown Response: ~p, ~p, ~p", [Type, ID, Request]),
	%TODO: Forward to the chat system.
	State.

%% ---------------------------------------------------------------------------------------------------------------------

handle_event({Type, ID, Request}, State) ->
	lager:warning("[Chat] Unknown Event: ~p, ~p, ~p", [Type, ID, Request]),
	%TODO: Forward to the chat system.
	State.

