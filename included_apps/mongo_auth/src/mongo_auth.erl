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

-record(state, {
			host,
			conn_pool
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
	
	%TODO: Get this from our configuration
	% Connect to mongodb
	Host = {localhost, 27017},
	Pool = resource_pool:new (mongo:connect_factory (Host), 10),
	State = #state{
		host = Host,
		conn_pool = Pool
	},
	{ok, State};


init(Args) ->
	?debug("Starting MongoDB Auth Plugin"),
	ok = start_app(mongo_auth),
	% Start our gen_server
	Pid = mongo_auth_sup:start_server(Args),
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
	case get_account_info(Username, State) of
		{error, Reason} ->
			{deny, Reason};

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

%% @doc Retrieve user information from mongo
handle_userinfo(Username, State) ->
	case get_account_info(Username, State) of
		{error, Reason} ->
			undefined;

		Account ->
			?info("Got Account Record: ~p", [Account]),
			Account
	end.

%TODO: This should be moved out into a header.
%% @doc Start an application, including it's dependancies
start_app(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {not_started, Dependency}} ->
			ok = start_app(Dependency),
			start_app(App)
	end.

%% @doc Gets a connection to the mongo database
get_db(Pool) ->
	case resource_pool:get (Pool) of
		{ok, Conn} ->
			Conn;

		{error, Reason} ->
			?error("Error connecting to the mongo database: ~p", [Reason]),
			error
	end.

%% @doc Retrieve the account record from the database
get_account_info(Username, State) ->
	#state{conn_pool = Pool} = State,
	case get_db(Pool) of
		error ->
				{error, "Error contacting database."};
		Conn ->
			Results = mongo:do(safe, master, Conn, precursors_server, fun() -> 
				mongo:find_one(accounts, {name, Username}) 
			end),
			
			case Results of
				{failure, Reason} ->
					{error, Reason};
				{ok, Account} ->
					Account
			end
	end.

%% @doc
generate_hash(Password, Salt) ->
	Digest = erlsha2:sha256(<<Password/binary, Salt/binary>>),
	bin_to_hex_string(Digest).

bin_to_hex_string(Bin) ->
	bin_to_hex_string(Bin, []).

bin_to_hex_string(<<>>, Acc) ->
	lists:flatten(lists:reverse(Acc));

bin_to_hex_string(<<First/integer, Rest/binary>>, Acc) ->
	bin_to_hex_string(Rest, [io_lib:format("~2.16.0b", [First]) | Acc]).

