%%% @doc Convenience wrapper for working with accounts. Should become more useful once we add things like player mail
%%% and other account management features. For now, it's just wrappers around `pre_data' calls.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_account).

% API
-export([authenticate/2, get_by_email/1, get_by_id/1, create/4, delete/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Authenticates an account, given the account name, and password. Account name, in this case, will be the email
%% address.
-spec authenticate(AccountName :: binary(), Password :: binary()) -> 'ok'.
authenticate(AccountName, Password) ->
	Result = get_by_email(AccountName),
	case Result of
		account_not_found ->
			account_not_found;
		{ok, Account} ->
			TestHash = pre_hash:hash(Password, pre_rec_account:hash_data(Account)),
			DbHash = pre_rec_account:password(Account),
			if
				TestHash =:= DbHash ->
					ok;
				true ->
					not_matched
			end
	end.

%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets an account by email address. Returns an account.
-spec get_by_email(EmailAddress :: binary()) -> {'ok', tuple()}.
get_by_email(EmailAddress) ->
	{ok, Rec} = pre_mnesia:search([{account, EmailAddress}]),
	case Rec of
		[] -> account_not_found;
		_ -> {ok, Rec}
	end.


%% @doc Gets an account by id. Returns an account.
	-spec get_by_id(AccountID :: binary()) -> {'ok', tuple()}.
get_by_id(AccountID) ->
	{ok, Rec} = pre_mnesia:search([{account, AccountID}]),
	case Rec of
		[] -> account_not_found;
		_ -> {ok, Rec}
	end.

%% @doc Creates a new account. Returns the newly created account.
-spec create(Email :: string(), RealName :: string(), NickName :: string(), Password :: string()) -> {'ok', tuple()}.
create(Email, RealName, NickName, Password) ->
	{HashedPassword, Credential} = pre_hash:hash(Password),
	pre_rec_account:new(Email, RealName, NickName, HashedPassword, Credential).


%% @doc Removed an account.
-spec delete(AccountID :: binary()) -> {'ok'}.
delete(AccountID) ->
	pre_mnesia:delete(account, AccountID).

