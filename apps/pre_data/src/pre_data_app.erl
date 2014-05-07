%% @doc Application callback module for pre_data. Is it not nifty?
-module(pre_data_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @hidden
start(_StartType, _StartArgs) ->
    pre_data_sup:start_link().

%% @hidden
stop(_State) ->
    ok.
