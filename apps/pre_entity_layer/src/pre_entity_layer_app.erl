%%% @doc The Precursors entity layer application callback module.
%%%

-module(pre_entity_layer_app).

-behaviour(application).

% Application callbacks
-export([start/2, stop/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% Application callbacks
%% --------------------------------------------------------------------------------------------------------------------

%% @hidden
start(_StartType, _StartArgs) ->
    pre_entity_engine_sup:start_link().

%% @hidden
stop(_State) ->
    ok.
