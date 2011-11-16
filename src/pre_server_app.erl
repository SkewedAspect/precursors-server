%% @doc Precursors Server application.  This starts the top level
%% supervisor, as well as provides useful stateless utility functions.
-module(pre_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% utiility functions
-export([priv_dir/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    pre_server_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Get the priv dir of the application even if it's running via
%% devboot.
priv_dir() ->
	case code:priv_dir(precursors_server) of
		{error,bad_name} -> "./priv";
		Else -> Else
	end.
