%%% @doc A simple behavior that defines a channel callback.

-module(pre_gen_channel).

-include("pre_client.hrl").

% Handles incoming event messages.
-callback handle_event(Channel :: binary(), Request :: term(), State :: #client_state{}) ->
	State :: #client_state{}.

% Handles incoming request messages.
-callback handle_request(Channel :: binary(), ID :: any(), Request :: term(), State :: #client_state{}) ->
	State :: #client_state{}.

% Handles incoming response messages.
-callback handle_response(Channel :: binary(), ID :: any(), Request :: term(), State :: #client_state{}) ->
	State :: #client_state{}.

