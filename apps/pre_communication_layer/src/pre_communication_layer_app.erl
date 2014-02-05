%%% @doc The Precursors communication layer application.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_communication_layer_app).

-behaviour(application).

% Application callbacks
-export([start/2, stop/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% Application callbacks
%% --------------------------------------------------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    pre_communication_layer_sup:start_link().

stop(_State) ->
    ok.
