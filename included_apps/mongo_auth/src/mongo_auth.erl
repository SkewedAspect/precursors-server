%% ----------------------------------------------------------------------------
%% @doc Precursors Server MongoDB based authentication module.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ----------------------------------------------------------------------------
-module(mongo_auth).
%-behavior(pre_gen_auth).

% -----------------------------------------------------------------------------

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
		code_change/3]).

% External AI
-export([start_link/1, get_user/2, handle_authentication/3]).


% -----------------------------------------------------------------------------

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").

-record(state, {}).

%% ----------------------------------------------------------------------------
%% External API
%% ----------------------------------------------------------------------------

start_link(Args) ->
	Result = gen_server:start_link(?MODULE, {gen_server, Args}, []),
	?debug("start_link result: ~p", [Result]),
	Result.


handle_authentication(Username, Password, Pid) ->
	gen_server:call(Pid, {auth, {Username, Password}}).


get_user(Username, Pid) ->
	gen_server:call(Pid, {user, Username}).


%% ----------------------------------------------------------------------------
%% gen_server
%% ----------------------------------------------------------------------------

init({gen_server, Args}) ->
	{ok, Args};

init(Args) ->
	?debug("Starting MongoDB Auth Plugin"),
	ok = start_app(mongo_auth),
	Pid = mongo_auth_sup:start_server(Args),
	{ok, Pid}.

% -----------------------------------------------------------------------------

handle_call({user, _Username}, _From, State) ->
	{reply, undefined, State};

handle_call({auth, {_Username, _Password}}, _From, State) ->
	{reply, {deny, "Reasons not yet implemented."}, State};

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

start_app(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {not_started, Dependency}} ->
			ok = start_app(Dependency),
			start_app(App)
	end.

%TODO: Do something.

