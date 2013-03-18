%%% @doc The behavior for all of our entity behavior modules. Provides a basic interface that we can count on.

-module(entity_behavior).
-export([behaviour_info/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

behaviour_info(callbacks) ->
    [{simulate, 1}];

behaviour_info(_) ->
    undefined.

%% --------------------------------------------------------------------------------------------------------------------

