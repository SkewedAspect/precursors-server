%% ----------------------------------------------------------------------------
%% @doc Precursors Server config based authentication module.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ----------------------------------------------------------------------------
-module(config_auth).
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

-record(state, {
		auth = {allow, all} :: {allow | deny, [] | all}
		}).

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
	Auth = case proplists:get_value(allow, Args) of
		true ->
			{allow, all};
		
		undefined ->
			case proplists:get_value(deny, Args) of
				true ->
					{deny, all};
				
				undefined ->
					% This is the default: allow everyone. Yes, I know this is the
					% deny case. however, if we have no lists defined, then we go to
					% the default, which is _allow_.
					{allow, all};
				
				Else ->
					{deny, Else}
			end;
		Else ->
			{allow, Else}
	end,

	?info("Auth is: ~p", [Auth]),

	State = #state{
		auth = Auth 
	},
	{ok, State};

init(Args) ->
	?debug("Starting MongoDB Auth Plugin"),
	ok = start_app(config_auth),

	% Start our gen_server
	Pid = config_auth_sup:start_server(Args),
	{ok, Pid}.


% -----------------------------------------------------------------------------

handle_call({user, Username}, _From, State) ->
	Info = handle_userinfo(Username, State),
	{reply, Info, State};

handle_call({auth, {Username, Password}}, _From, State) ->
	Auth = handle_auth(Username, Password, State),
	{reply, Auth, State};

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

%% @doc Authenticate user against mongo
handle_auth(Username, Password, State) ->
	UserBin = binary_to_atom(Username, latin1),
	case State#state.auth of
		{allow, all} ->
			allow;

		{deny, all} ->
			{deny, "Authorization Denied."};

		{allow, Allowed} ->
			case lists:member(UserBin, Allowed) of
				true ->
					?info("allow: Allowing member."),
					allow;
				false ->	
					?info("allow: member not in list."),
					{deny, "Authorization Denied."}
			end;

		{deny, Denied} ->
			case lists:member(UserBin, Denied) of
				true ->
					?info("deny: Denying member."),
					{deny, "Authorization Denied."};
				false ->	
					?info("deny: member not in list."),
					allow
			end
	end.

%% @doc Retrieve user information from mongo
handle_userinfo(Username, State) ->
	undefined.

%TODO: This should be moved out into a header.
%% @doc Start an application, including it's dependancies
start_app(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok;
		{error, {not_started, Dependency}} ->
			ok = start_app(Dependency),
			start_app(App)
	end.
