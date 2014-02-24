%%% @doc A simple behavior that defines a channel callback.
%%% --------------------------------------------------------------------------------------------------------------------

-module(pre_channel).

% Behavior
-export([behaviour_info/1]).

%% ---------------------------------------------------------------------------------------------------------------------

behaviour_info(callbacks) ->
	[{handle_request, 2}, {handle_response, 2}, {handle_event, 2}];

behaviour_info(_) ->
	undefined.