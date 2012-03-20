%% ----------------------------------------------------------------------------
%% @doc Precursors Server MongoDB based authentication module.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ----------------------------------------------------------------------------
-module(mongo_auth).
-behavior(pre_gen_auth).

% -----------------------------------------------------------------------------

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
		code_change/3]).

% External AI
-export([start_link/0, get_user/2, handle_authentication/3]).


% -----------------------------------------------------------------------------

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").

-record(state, {servers = dict:new()}).

%% ----------------------------------------------------------------------------
%% External API
%% ----------------------------------------------------------------------------

start_link() ->
	Result = gen_server:start_link(?MODULE, [], []),
	?debug("start_link result: ~p", [Result]),
	Result.


handle_authentication(Username, Password, BackendInfo) ->
	allow.


get_user(Username, BackendInfo) ->
	undefined.


%% ----------------------------------------------------------------------------
%% gen_server
%% ----------------------------------------------------------------------------

init([]) ->
	?debug("Starting WeatherPlugin"),
	erkbot_event:add_plugin(?MODULE, self()),
	State = #state{},
	{ok, State}.

% -----------------------------------------------------------------------------

handle_call(_, _From, State) ->
    {reply, invalid, State}.

% -----------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

% -----------------------------------------------------------------------------

handle_info(_, State) ->
    {noreply, State}.

% -----------------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.

code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% ----------------------------------------------------------------------------
%% Internal API
%% ----------------------------------------------------------------------------

%TODO: Do something.

