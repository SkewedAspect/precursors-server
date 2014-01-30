%% @doc Gen_event manager and behavior definition for authentication
%% backends.  Can itself be an authentication backend.  As such, the
%% gen_event callback functions have two modes:  callback wrapper and
%% direct responder.  Callback wrapping is simply to enforce behavior on
%% other modules.  The actually implementation of the authentication
%% backends could be something completely different (gen_server is a solid
%% choice).
%%
%% There are 4 callbacks in the behavior.
%%
%% <b>init(InitArgs) -> {ok, BackendInfo}</b>
%%
%% Types:  <ul>
%% <li>InitArgs :: any()</li>
%% <li>BackendInfo :: any()</li>
%% </ul>
%%
%% BackendInfo is used for subsequent `handle_authentication/3' and
%% `get_user/2' calls.
%%
%% <b>handle_authentication(Username, Password, BackendInfo) -> Authy</b>
%%
%% Types: <ul>
%% <li>Username :: string()</li>
%% <li>Password :: string()</li>
%% <li>BackendInfo :: any()</li>
%% <li>Authy :: 'allow' | {'deny', Reason} | 'undefined' | Error</li>
%% <li>Reason :: string()</li>
%% <li>Error :: any()</li>
%% </ul>
%%
%% When a user needs to be authenticated, each authentication backend in
%% order of priority.  `Password' is plaintext.  `BackendInfo' what was
%% returned from the init function.  If a backend returns `Error' 3 times,
%% that backend is removed.
%%
%% <b>get_user(Username, BackendInfo) ->  UserInfo</b>
%%
%% Types:  <ul>
%% <li>Username :: string()</li>
%% <li>BackendInfo :: any()</li>
%% <li>UserInfo :: any()</li>
%% </ul>
%%
%% Return any information the backend may have on the user.  `undefined' is
%% taken to mean there was no information.
%%
%% <b>terminate(Cause, BackendInfo) -> any()</b>
%%
%% Types: <ul>
%% <li>Cause :: any()</li>
%% <li>BackendInfo :: any()</li>
%% </ul>
%%
%% The return value is ignored.  When the authentication system is ending,
%% or the authentication backend is removed from service, this is called
%% with the `Cause'.  `BackendInfo' is what was returned from the init
%% function.

-module(pre_gen_auth).
-behavior(gen_event).

-include("internal_auth.hrl").
-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
	-compile([export_all]).
-endif.

% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3, format_status/2]).
% API
-export([start_link/0, start_link/1, behavior_info/1, add_backend/3,
	authenticate/2, get_user/3, remove_backend/2]).
% when self as backend
-export([handle_authentication/3, get_user/2]).

-record(state, {
	callback,
	substate,
	error_count = 0
}).

% types
%-type(cause() :: string()).
%-type(backend_error() :: any()).
%-type(username() :: string()).
%-type(password() :: string()).
%-type(backend_identifier() :: any()).
%-type(backend_init_reply() :: {'ok', backend_identifier()}).
%-type(backend_auth_reply() :: {'allow', username()} | {'deny', cause()} | backend_error()).

%% ==================================================================
%% API
%% ==================================================================

%% @doc Starts the authentication system with only the local auther.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() -> start_link([{pre_gen_auth, 100, ?MODULE}]).

%% @doc Starts the authentication system with a list of the given authers.
-type(callback_module() :: atom()).
-type(priority() :: integer()).
-type(start_args() :: any()).
-type(backend() :: {callback_module(), priority(), start_args()}).
-type(backends() :: [backend()]).
-spec(start_link/1 :: (Auths :: backends()) -> {'ok', pid()}).
start_link(Auths) ->
	Out = gen_event:start_link({local, ?MODULE}),
	AddRes = [gen_event:add_handler(?MODULE, {?MODULE, {Priority, Callback}}, {Callback, Args}) ||
		{Callback, Priority, Args} <- Auths],
	lager:info("Adding authentication backends:  ~p", [AddRes]),
	Out.

%% @doc Query authentication backends to see if a user is allowed or not.
%% Authentication backends are queried in order of priority, lowest to
%% highest.  Query stops at the first backend that returns a definitive
%% allow or deny.
-spec(authenticate/2 :: (Username :: string(), Password :: string()) ->
	{'allow', binary()} | {'deny', string()}).
authenticate(Username, Password) ->
	Handlers = gen_event:which_handlers(?MODULE),
	Handlers0 = lists:keysort(2, Handlers),
	authenticate(Handlers0, Username, Password).

%% @doc As the specific authentication backend for all information they
%% have on the given user.
-spec(get_user/3 :: (Username :: string(),
	Handler :: atom(),
	Priority :: integer()) -> any()).
get_user(Username, Handler, Priority) ->
	Handler0 = {?MODULE, {Priority, Handler}},
	gen_event:call(?MODULE, Handler0, {get_user, Username}).

%% @doc Add a backend to an already running authentication manager.
-spec(add_backend/3 :: (Callback :: atom(), Priority :: integer(),
	Args :: any()) -> 'ok' | {'EXIT', any()} | {'error', any()}).
add_backend(Callback, Priority, Args) ->
	gen_event:add_handler(?MODULE, {?MODULE, {Priority, Callback}}, {Callback, Args}).

%% @doc Removes a backend from the list.
-spec(remove_backend/2 :: (Callback :: atom(), Priority :: integer()) ->
	'ok').
remove_backend(Callback, Priority) ->
	gen_event:delete_handler(?MODULE, {?MODULE, {Priority, Callback}}, removed),
	ok.

%% @hidden
behavior_info(exports) ->
	[{init, 1},
	{handle_authentication, 3},
	{get_user, 2},
	{terminate, 2}];
behavior_info(_) ->
	undefined.

%% @hidden
handle_authentication(Username, Password, undefined) ->
%	MatchSpec = [
%		{#user_auth{username = '$1', password = '$2', _ = '_'},
%		[
%			{'=:=', '$1', Username},
%			{'=:=', '$2', Password}
%		],
%		['$1']
%	],
	Rec = #user_auth{username = Username, _ = '_'},
	case mnesia:dirty_match_object(Rec) of
		[] ->
			undefined;
		[#user_auth{password = DP} | _] when DP =/= Password ->
			{deny, "invalid password"};
		[_Account | _] ->
			{allow, Username}
	end.

%% @hidden
get_user(Username, undefined) ->
	Rec = #user_auth{username = Username, _ = '_'},
	case mnesia:dirty_match_object(Rec) of
		[] -> undefined;
		[X] -> X
	end.

%% ==================================================================
%% gen_event callbacks
%% ==================================================================

%% ------------------------------------------------------------------
%% init
%% ------------------------------------------------------------------

%% @private
init({Callback, Args}) ->
	case Callback:init(Args) of
		{ok, Substate} ->
			{ok, #state{callback = Callback, substate = Substate}};
		Else ->
			{error, Else}
	end;

init(?MODULE) ->
	build_tables(),
	{ok, undefined}.

%% ------------------------------------------------------------------
%% handle_event
%% ------------------------------------------------------------------

%% @hidden
handle_event(Event, State) ->
	lager:debug("Ignoring event:  ~p", [Event]),
	{ok, State}.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @hidden
handle_call({authentication, _Username, _Password}, undefined) ->
	% TODO mnesia backed
	{ok, undefined, undefined};

handle_call({authentication, Username, Password}, State) ->
	#state{callback = Callback, substate = Substate, error_count = Errs} = State,
	Out = Callback:handle_authentication(Username, Password, Substate),
	ErrCount = case Out of
		{allow, _} -> Errs;
		{deny, _} -> Errs;
		undefined -> Errs;
		_Err -> Errs + 1
	end,
	case ErrCount of
		3 ->
			lager:notice("Backend ~s returned its 3rd and final error ~p", [Callback, Out]),
			remove_handler;
		Errs ->
			{ok, Out, State};
		_ ->
			lager:notice("Backend ~s returned an error ~p; this is error ~p", [Callback, Out, ErrCount]),
			{ok, Out, State#state{error_count = ErrCount}}
	end;

handle_call({get_user, _Username}, undefined) ->
	% TODO mnesia backed
	{ok, undefined, undefined};

handle_call({get_user, Username}, State) ->
	#state{callback = Callback, substate = Substate} = State,
	Out = Callback:get_user(Username, Substate),
	{ok, Out, State};

handle_call(Req, State) ->
	lager:info("unhandled call:  ~p", [Req]),
	{ok, {error, invalid}, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

%% @hidden
handle_info(Msg, State) ->
	lager:info("unhandled info:  ~p", [Msg]),
	{noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

%% @hidden
terminate(Why, State) ->
	lager:info("Going down:  ~p", [Why]),
	#state{callback = Callback, substate = Substate} = State,
	Callback:terminate(Why, Substate).

%% ------------------------------------------------------------------
%% code_change
%% ------------------------------------------------------------------

%% @hidden
code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% format_status
%% ------------------------------------------------------------------

%% @hidden
format_status(Opt, [PDict, State]) ->
	[{data, [{"State", State}]}].

%% ==================================================================
%% Internal Functions
%% ==================================================================

authenticate([], _Username, _Password) ->
	% if no-one knows, deny
	{deny, "No backends definitive"};

authenticate([Handler | Tail], UserOrNick, Password) ->
	case gen_event:call(?MODULE, Handler, {authentication, UserOrNick, Password}) of
		{allow, Username} ->
			{allow, Username};
		{deny, Msg} ->
			{deny, Msg};
		undefined ->
			authenticate(Tail, UserOrNick, Password);
		Else ->
			lager:info("Handler ~p returned a bad value ~p", [Handler, Else]),
			authenticate(Tail, UserOrNick, Password)
	end.

build_tables() ->
	Create = mnesia:create_table(user_auth, [
		{attributes, record_info(fields, user_auth)}
	]),
	case Create of
		{atomic, ok} -> ok;
		{aborted, already_exits} -> ok;
		{aborted, Else} ->
			lager:warning("Could not build user_auth table.  This will die later."),
			{error, Else}
	end.

proplist_to_user_auth(Proplist) ->
	BaseRec = #user_auth{},
	KeyPos = record_info(fields, user_auth),
	{error, nyi}.
