-module(pre_channel_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    pre_channel_sup:start_link().

stop(_State) ->
    ok.
