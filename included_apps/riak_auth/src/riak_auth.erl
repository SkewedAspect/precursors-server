%% ------------------------------------------------------------------------
%% @doc Riak-based authentication module for Precursors Server.
%%
%% @copyright 2012 Christopher S. Case
%% Licensed under the MIT license; see the LICENSE file for details.
%% ------------------------------------------------------------------------
-module(riak_auth).
%-behavior(pre_gen_auth).

% -------------------------------------------------------------------------

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
		code_change/3]).

% External AI
-export([start_link/1, get_user/2, handle_authentication/3]).


% -------------------------------------------------------------------------

-include_lib("precursors_server/include/log.hrl").
-include_lib("precursors_server/include/internal_auth.hrl").

-record(state, {
			host,
			conn_pool,
			username,
			password,
			port :: riakc_pb_socket:portnum(),
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
	State = #state{host=["localhost", 27017]},
	init(connect, State);

init(Args) ->
	?debug("Starting Riak Auth Plugin"),
	ok = pre_util:start_app(riak_auth),

	% Start our gen_server
	Pid = riak_auth_sup:start_server(Args),
	{ok, Pid}.


init(connect, State) ->
	% Connect to riakc
	Host = State#state.host,
	Port = State#state.port,
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

handle_call(_, _From, State) ->
    {reply, invalid, State}.

% -------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

% -------------------------------------------------------------------------

handle_info(_, State) ->
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
	case get_account_info(Username, State) of
		{error, not_found} ->
			undefined;
		{error, Reason} ->
			case is_atom(Reason) of
				true ->
					{deny, "Database Error: " ++ atom_to_list(Reason)};
				false ->
					{deny, "Database Error: " ++ Reason}
			end;

		{Account} ->
			?info("Got Account Record: ~p", [Account]),
			PasswordHash = bson:at(password_hash, Account),
			PasswordSalt = bson:at(password_salt, Account),
			AuthPasswordHash = string:to_lower(generate_hash(Password, PasswordSalt)),
			PassHashString = binary_to_list(PasswordHash),
			?info("Got db: ~p, hash: ~p", [PassHashString, AuthPasswordHash]),
			case PassHashString of
				AuthPasswordHash ->
					allow;

				_ ->
					{deny, "Invalid Password"}
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


%% @doc Retrieve the account record from the database
get_account_info(Username, State) ->
	#state{
		riak_conn = RiakConn
	} = State,

	case riakc_pb_socket:get(RiakConn, <<"users">>, Username) of
		{ok, RiakCObj} ->
			riakc_obj:get_value(RiakCObj);
		Error ->
			Error
	end.


%% @doc Generate hash from the password and salt
generate_hash(Password, Salt) ->
	Digest = erlsha2:sha256(<<Password/binary, Salt/binary>>),
	bin_to_hex_string(Digest).


bin_to_hex_string(Bin) ->
	bin_to_hex_string(Bin, []).


bin_to_hex_string(<<>>, Acc) ->
	lists:flatten(lists:reverse(Acc));


bin_to_hex_string(<<First/integer, Rest/binary>>, Acc) ->
	bin_to_hex_string(Rest, [io_lib:format("~2.16.0b", [First]) | Acc]).
