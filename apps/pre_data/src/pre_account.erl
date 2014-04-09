%%% @doc Convenience wrapper for working with accounts. Should become more useful once we add things like player mail
%%% and other account management features. For now, it's just wrappers around `pre_data' calls.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_account).

-compile([{parse_transform, rec2json}]).

-define(transact(Thing), pre_data:transaction(fun() -> Thing end)).

-record(pre_account, {
	id :: any(),                % auto generated id
	email :: binary(),          % der email, secondary index
	real_name :: binary(),      % optional
	nickname :: binary(),       % unique constraint
	password :: binary(),       % hash of the password
	hash_data :: any(),         % the hash algorthm used and any args needed.
	created :: erlang:timestamp(),
	updated :: erlang:timestamp()
}).

% API
-export([authenticate/2, get_by_email/1, get_by_id/1, create/4, delete/1, save/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Authenticates an account, given the account name, and password. Account name, in this case, will be the email
%% address.
-spec authenticate(AccountName :: binary(), Password :: binary()) -> 'ok' | {'error', term()}.
authenticate(AccountName, Password) ->
	Result = get_by_email(AccountName),
	case Result of
		{ok, Account} ->
			lager:debug("Auth success: ~p", [Account]),
			{TestHash, _} = pre_hash:hash(Password, Account:hash_data()),
			DbHash = Account:password(),
			if
				TestHash =:= DbHash ->
					ok;
				true ->
					{error, not_matched}
			end;
		Else -> Else
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets an account by email address. Returns an account.
-spec get_by_email(EmailAddress :: binary()) -> {'ok', tuple()}.
get_by_email(EmailAddress) ->
	Got = ?transact(pre_data:search(pre_account, [{email, EmailAddress}])),
	case Got of
		{ok, []} -> {error, notfound};
		{ok, [R | _]} -> {ok, R};
		Else -> Else
	end.


%% @doc Gets an account by id. Returns an account.
-spec get_by_id(AccountID :: binary()) -> {'ok', tuple()}.
get_by_id(AccountID) ->
	?transact(pre_data:get_by_id(pre_account, AccountID)).

%% @doc Creates a new account. Returns the newly created account.
-spec create(Email :: binary(), RealName :: binary(), NickName :: binary(), Password :: binary()) -> {'ok', tuple()} | {error, term()}.
create(Email, RealName, NickName, Password) ->
	case get_by_email(Email) of
		{error, notfound} ->
			{HashedPassword, Credential} = pre_hash:hash(Password),

			New = #pre_account{
				email = Email,
				real_name = RealName,
				nickname = NickName,
				password = HashedPassword,
				hash_data = Credential
			},

			?transact(pre_data:save(New));

		{ok, _Char} ->
			{error, already_exists}
	end.


%% @doc Saves a account. Returns the account.
-spec save(Account :: #pre_account{}) -> {ok, #pre_account{}} | {error, term()}.
save(Account) ->
	?transact(pre_data:save(Account)).


%% @doc Removed an account.
-spec delete(AccountID :: any()) -> 'ok'.
delete(AccountID) ->
	?transact(pre_data:delete(pre_account, AccountID)).

