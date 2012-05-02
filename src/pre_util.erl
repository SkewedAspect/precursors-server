%% ----------------------------------------------------------------------------
%% @doc Miscellaneous utilities, intended for use by included apps, others.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ----------------------------------------------------------------------------
-module(pre_util).

% -----------------------------------------------------------------------------

% gen_server
-export([start_app/1, start_apps/1]).

% -----------------------------------------------------------------------------

-include("log.hrl").

%% ----------------------------------------------------------------------------
%% External API
%% ----------------------------------------------------------------------------

%% @doc Start an application and all of it's required dependancies. This simply
%% tries to start the application, and if it fails, it catches the error and if
%% the error is an unstarted dependancy, it attempts to start that application.
start_app(App) ->
	case application:start(App) of
		ok ->
			?info("Application '~p' started", [App]),
			ok;

		{error, {already_started, App}} ->
			?error("Application '~p' already started.", [App]),
			ok;
		
		{error, {not_started, Dependency}} ->
			?debug("Unstarted Dependancy '~p' detected, starting...", [Dependency]),
			ok = start_app(Dependency),
			start_app(App)
	end.

%% @doc Starts a list of applications (generally plugins). (Calls start_app under
%% the hood.)
start_apps([]) ->
	ok;

start_apps([App | Rest]) ->
	?info("Starting application '~p'.", [App]),
	start_app(App),
	start_apps(Rest).
