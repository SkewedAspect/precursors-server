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

-include("log.hrl").
-include("internal_auth.hrl").

-ifdef(TEST).
	-compile([export_all]).
-endif.

% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3, format_status/2]).
% API
-export([start_link/0, start_link/1, behavior_info/1, add_backend/3,
	authentication/2, get_user/2]).

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
%-type(backend_auth_reply() :: 'allow' | {'deny', cause()} | backend_error()).

%% ==================================================================
%% API
%% ==================================================================

%% @doc Starts the authentication system with only the local auther.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() -> start_link([{pre_gen_auth, 100, self}]).

%% @doc Starts the authentication system with a list of the given authers.
-type(callback_module() :: atom()).
-type(priority() :: integer()).
-type(start_args() :: any()).
-type(backend() :: {callback_module(), priority(), start_args()}).
-type(backends() :: [backend()]).
-spec(start_link/1 :: (Auths :: backends()) -> {'ok', pid()}).
start_link(Auths) ->
	Out = gen_event:start_link(?MODULE),
	AddRes = [gen_event:add_handler(?MODULE, {?MODULE, {Priority, Callback}}, {Callback, Args}) ||
		{Callback, Priority, Args} <- Auths],
	?info("Adding authentication backends:  ~p", [AddRes]),
	Out.

%% @doc Query authentication backends to see if a user is allowed or not.
%% Authentication backends are queried in order of priority, lowest to
%% highest.  Query stops at the first backend that returns a definitive
%% allow or deny.
-spec(authentication/2 :: (Username :: string(), Password :: string()) ->
	'allow' | {'deny', string()}).
authentication(Username, Password) ->
	Handlers = gen_event:which_handlers(?MODULE),
	Handlers0 = lists:keysort(2, Handlers),
	authentication(Username, Password, Handlers0).

%% @doc As the specific authentication backend for all information they
%% have on the given user.
-spec(get_user/2 :: (Username :: string(),
	Handler :: {integer(), atom()}) -> any()).
get_user(Username, Handler) ->
	Handler0 = {?MODULE, Handler},
	gen_event:call(?MODULE, Handler0, {get_user, Username}).

%% @doc Add a backend to an already running authentication manager.
-spec(add_backend/3 :: (Callback :: atom(), Priority :: integer(),
	Args :: any()) -> 'ok' | {'EXIT', any()} | {'error', any()}).
add_backend(Callback, Priority, Args) ->
	gen_event:add_handler(?MODULE, {?MODULE, {Priority, Callback}}, {Callback, Args}).

%% @hidden
behavior_info(exports) ->
	[{init, 1},
	{handle_authentication, 3},
	{get_user, 2},
	{terminate, 2}];
behavior_info(_) ->
	undefined.

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

init(self) ->
	build_tables(),
	{ok, undefined}.

%% ------------------------------------------------------------------
%% handle_event
%% ------------------------------------------------------------------

%% @hidden
handle_event(Event, State) ->
	?debug("Ignoring event:  ~p", [Event]),
	{ok, State}.

%% ------------------------------------------------------------------
%% handle_call
%% ------------------------------------------------------------------

%% @hidden
handle_call({authentication, Username, Password}, undefined) ->
	% TODO mnesia backed
	{reply, undefined, undefined};

handle_call({authentication, Username, Password}, State) ->
	#state{callback = Callback, substate = Substate, error_count = Errs} = State,
	Out = Callback:handle_authentication(Username, Password, Substate),
	Errcount = case Out of
		allow -> Errs;
		{deny, _} -> Errs;
		undefined -> Errs;
		_Err -> Errs + 1
	end,
	case Errcount of
		3 ->
			?notice("backed ~s returned it's 3rd and final error ~p", [Callback, Out]),
			remove_handler;
		Errs ->
			{reply, Out, State};
		_ ->
			?notice("backend ~s returned an error ~p; this is error ~p", [Callback, Out, Errcount]),
			{reply, Out, State#state{error_count = Errcount}}
	end;

handle_call({get_user, Username}, undefined) ->
	% TODO mnesia backed
	{reply, undefined, undefined};

handle_call({get_user, Username}, State) ->
	#state{callback = Callback, substate = Substate} = State,
	Out = Callback:get_user(Username, Substate),
	{reply, Out, State};

handle_call(Req, State) ->
	?info("unhandled call:  ~p", [Req]),
	{reply, {error, invalid}, State}.

%% ------------------------------------------------------------------
%% handle_info
%% ------------------------------------------------------------------

%% @hidden
handle_info(Msg, State) ->
	?info("unhandled info:  ~p", [Msg]),
	{noreply, State}.

%% ------------------------------------------------------------------
%% terminate
%% ------------------------------------------------------------------

%% @hidden
terminate(Why, State) ->
	?info("Going down:  ~p", [Why]),
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

authentication([], _Username, _Password) ->
	% if no-one knows, deny
	{deny, "No backends definitive"};

authentication([Handler | Tail], Username, Password) ->
	case gen_event:call(?MODULE, Handler, {authentication, Username, Password}) of
		allow ->
			allow;
		{deny, Msg} ->
			{deny, Msg};
		undefined ->
			authentication(Tail, Username, Password);
		Else ->
			?info("Handler ~p returned an bad value ~p", [Handler, Else]),
			authentication(Tail, Username, Password)
	end.

build_tables() ->
	Create = mnesia:create_table(user_auth, [
		{attributes, record_info(fields, user_auth)}
	]),
	case Create of
		{atomic, ok} -> ok;
		{aborted, already_exits} -> ok;
		{aborted, Else} ->
			?warning("Could not build user_auth table.  This will die later."),
			{error, Else}
	end.

proplist_to_user_auth(Proplist) ->
	BaseRec = #user_auth{},
	KeyPos = record_info(fields, user_auth),
	{error, nyi}.

%% ==================================================================
%% API
%% ==================================================================


%% ==================================================================
%% API
%% ==================================================================

%% ==================================================================
%% API
%% ==================================================================

%% ==================================================================
%% API
%% ==================================================================

%% ==================================================================
%% API
%% ==================================================================


%% ------------------------------------------------------------------
%% 
%% ------------------------------------------------------------------
