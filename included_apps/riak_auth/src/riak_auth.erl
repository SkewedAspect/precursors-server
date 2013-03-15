%% ------------------------------------------------------------------------
%% @doc Riak-based authentication module for Precursors Server.
%%
%% @copyright 2012 Christopher S. Case and David H. Bronke
%% Licensed under the MIT license; see the LICENSE file for details.
%% ------------------------------------------------------------------------
-module(riak_auth).
%-behavior(pre_gen_auth).

% -------------------------------------------------------------------------
%Testing:

%{ok, State} = riak_auth:init(connect, {state, "localhost", 8081, undefined, undefined, undefined}).
%riak_auth:get_account_info(<<"david.bronke@g33xnexus.com">>, State).
%riak_auth:get_account_credentials(<<"david.bronke@g33xnexus.com">>, State).

% -------------------------------------------------------------------------

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).

% External API
-export([start_link/1, get_user/2, handle_authentication/3]).

%XXX:TESTING
-export([init/2, get_account_info/2, get_account_credentials/2]).


% -------------------------------------------------------------------------

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").

% Default Riak connection parameters:
-define(DEFAULT_RIAK_PB_PORT, 8087).

% Default PBKDF2 values to the ones used by WPA2:
-define(DEFAULT_PBKDF2_ITERATIONS, 4096).
-define(DEFAULT_PBKDF2_DERIVED_LENGTH, 32).
-define(DEFAULT_PBKDF2_PRF, <<"HMAC+SHA256">>).

-record(state, {
			host = "localhost",
			port = ?DEFAULT_RIAK_PB_PORT :: riakc_pb_socket:portnum(),
			username,
			password,
			riak_conn :: pid()
		}).

%% ------------------------------------------------------------------------
%% External API
%% ------------------------------------------------------------------------

start_link(Args) ->
	Result = gen_server:start_link(?MODULE, {gen_server, Args}, []),
	?debug("start_link result: ~p", [Result]),
	Result.


handle_authentication(Username, Password, Pid) ->
	gen_server:call(Pid, {auth, {Username, Password}}).


get_user(Username, Pid) ->
	gen_server:call(Pid, {user, Username}).


%% ------------------------------------------------------------------------
%% gen_server
%% ------------------------------------------------------------------------

init({gen_server, [Host, Port] = _Args}) ->
	State = #state{
		host = Host,
		port = Port
	},
	init(connect, State);

init({gen_server, [Host] = _Args}) ->
	State = #state{
		host = Host
	},
	init(connect, State);

init({gen_server, [Host, {Username, Password}] = _Args}) ->
	State = #state{
		host = Host,
		username = Username,
		password = Password
	},
	init(connect, State);

init({gen_server, _Args}) ->
	State = #state{},
	init(connect, State);

init(Args) ->
	?debug("Starting Riak Auth Plugin"),
	ok = pre_util:start_app(riak_auth),

	% Start our gen_server
	Pid = riak_auth_sup:start_server(Args),
	{ok, Pid}.


init(connect, State) ->
	% Connect to riakc
	#state{
		host = Host,
		port = Port
	} = State,
	{ok, RiakConn} = riakc_pb_socket:start_link(Host, Port),
	NewState = State#state{
		riak_conn = RiakConn
	},
	{ok, NewState}.

% -------------------------------------------------------------------------

handle_call({user, Username}, _From, State) ->
	Info = handle_userinfo(Username, State),
	{reply, Info, State};

handle_call({auth, {Username, Password}}, _From, State) ->
	Auth = handle_auth(Username, Password, State),
	{reply, Auth, State};

handle_call(Invalid, _From, State) ->
	?warning("Got invalid call: ~p", [Invalid]),
    {reply, invalid, State}.

% -------------------------------------------------------------------------

handle_cast(Invalid, State) ->
	?warning("Ignoring invalid cast: ~p", [Invalid]),
    {noreply, State}.

% -------------------------------------------------------------------------

handle_info(Invalid, State) ->
	?warning("Ignoring invalid info: ~p", [Invalid]),
    {noreply, State}.

% -------------------------------------------------------------------------

terminate(Reason, _State) ->
	?info("Terminating due to ~p.", [Reason]),
	ok.


code_change(_OldVersion, State, _Extra) ->
	{reply, State}.

%% ------------------------------------------------------------------------
%% Internal API
%% ------------------------------------------------------------------------

%% @doc Authenticate user against riak
handle_auth(Username, Password, State) ->
	case get_account_credentials(Username, State) of
		{error, not_found} ->
			?info("No account credentials found for Username ~p.", [Username]),
			undefined;

		{error, Reason} ->
			?info("Error getting account credentials for Username ~p: ~p", [Username, Reason]),
			case is_atom(Reason) of
				true ->
					{deny, "Database Error: " ++ atom_to_list(Reason)};
				false ->
					{deny, "Database Error: " ++ Reason}
			end;

		Credentials when is_list(Credentials) ->
			?info("Got credentials for account '~s': ~p", [Username, Credentials]),
			case lists:any(
					fun (CredentialProps) -> check_cred(Username, Password, CredentialProps) end,
					Credentials) of
				true ->
					allow;

				false ->
					{deny, "Incorrect Password"}
			end
	end.


%% @doc Retrieve user information from riak
handle_userinfo(Username, State) ->
	case get_account_info(Username, State) of
		{error, _Reason} ->
			undefined;

		Account ->
			?info("Got Account Record: ~p", [Account]),
			Account
	end.


%% @doc Check user credential
check_cred(_Username, CheckPassword, Credential) ->
	% Example of what we (should) get from the DB:
	% {
	%     "hash": "tNcJYbTF6jJAn6mLxjAZP5Nb+yJ5P8A72YJI57rtDDU=",
	%     "iterations": 20000.0,
	%     "prf": "HMAC+SHA256",
	%     "salt": "1yfcsqA9MYJz",
	%     "type": "local"
	% }
	StoredPasswordHash = proplists:get_value(hash, Credential),
	PasswordSalt = proplists:get_value(salt, Credential),
	Iterations = proplists:get_value(iterations, Credential, ?DEFAULT_PBKDF2_ITERATIONS),
	PseudoRandomFunction = proplists:get_value(prf, Credential, ?DEFAULT_PBKDF2_PRF),
	DerivedLength = ?DEFAULT_PBKDF2_DERIVED_LENGTH,  %FIXME: Determine this from the PRF!

	case PseudoRandomFunction of
		?DEFAULT_PBKDF2_PRF ->
			%FIXME: Use the PRF!
			{ok, CheckPasswordHash} = pbkdf2:pbkdf2(sha256, CheckPassword, PasswordSalt, Iterations, DerivedLength),
			CheckPasswordHashBase64 = base64:encode(CheckPasswordHash),
			?info("Client-provided hash: ~p; stored hash: ~p", [CheckPasswordHashBase64, StoredPasswordHash]),

			pbkdf2:compare_secure(StoredPasswordHash, CheckPasswordHashBase64);

		_ ->
			?warning("Incompatible PRF ~p (only ~p is supported)", [PseudoRandomFunction, ?DEFAULT_PBKDF2_PRF]),
			false
	end.


%% @doc Retrieve the account record from the database
get_account_info(Username, State) ->
	#state{
		riak_conn = RiakConn
	} = State,

	case binary:match(Username, <<$@>>) of
		nomatch ->
			case riakc_pb_socket:get_index(RiakConn, <<"account">>, <<"nickname_bin">>, Username) of
				{ok, RiakCObj} ->
					AccountBin = riakc_obj:get_value(RiakCObj),
					pre_json:to_term(AccountBin);
				Error ->
					Error
			end;
		_ ->
			case riakc_pb_socket:get(RiakConn, <<"account">>, Username) of
				{ok, RiakCObj2} ->
					Account2Bin = riakc_obj:get_value(RiakCObj2),
					pre_json:to_term(Account2Bin);
				Error2 ->
					Error2
			end
	end.


%% @doc Retrieve the matching credential records from the database
get_account_credentials(Username, State) ->
	#state{
		riak_conn = RiakConn
	} = State,

	Email = case binary:match(Username, <<$@>>) of
		nomatch ->
			case riakc_pb_socket:get_index(RiakConn, <<"account">>, <<"nickname_bin">>, Username) of
				{ok, RiakCObj} ->
					AccountBin = riakc_obj:get_value(RiakCObj),
					Account = pre_json:to_term(AccountBin),
					proplists:get_value(email, Account);
				Error ->
					Error
			end;
		_ -> Username
	end,

	case riakc_pb_socket:mapred(RiakConn,
			[{<<"account">>, Email}],
			[
				{link, <<"credential">>, '_', false},
				{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true}
				]
			) of
		{ok, [{1, Results}]} ->
			lists:map(
				fun (CredentialBin) ->
					pre_json:to_term(CredentialBin)
				end,
				Results
				);
		Error2 ->
			Error2
	end.
