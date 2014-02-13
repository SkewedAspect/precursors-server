%%% @doc The Precursors entity layer application.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_entity_layer_app).

-behaviour(application).

% Application callbacks
-export([start/2, stop/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% Application callbacks
%% --------------------------------------------------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    pre_entity_engine_sup:start_link().

stop(_State) ->
    ok.