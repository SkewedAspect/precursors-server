%%% @doc The Precursors communication layer application callback module.
%%%

-module(pre_communication_layer_app).

-behaviour(application).

% Application callbacks
-export([start/2, stop/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% Application callbacks
%% --------------------------------------------------------------------------------------------------------------------

%% @private
start(_StartType, _StartArgs) ->
    pre_communication_layer_sup:start_link().

%% @private
stop(_State) ->
    ok.
