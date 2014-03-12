%%% @doc Convenience wrapper for working with accounts. Should become more useful once we add things like player mail
%%% and other account management features. For now, it's just wrappers around `pre_data' calls.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_account).

-include_lib("eunit/include/eunit.hrl").

-define(t(Thing), pre_data:transaction(fun() -> Thing end)).

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
			{TestHash, _} = pre_hash:hash(Password, pre_rec_account:hash_data(Account)),
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
	{ok, Rec} = ?t(pre_data:search(pre_rec_account, [{email, EmailAddress}])),
	case Rec of
		[] -> account_not_found;
		[R | _] ->
			{ok, R}
	end.


%% @doc Gets an account by id. Returns an account.
	-spec get_by_id(AccountID :: binary()) -> {'ok', tuple()}.
get_by_id(AccountID) ->
	Got = ?t(pre_data:get_by_id(pre_rec_account, AccountID)),
	case Got of
		{error, notfound} -> account_not_found;
		_ -> {ok, Got}
	end.

%% FIXME: De-duplicate before creating!
%% @doc Creates a new account. Returns the newly created account.
-spec create(Email :: binary(), RealName :: binary(), NickName :: binary(), Password :: binary()) -> {'ok', tuple()}.
create(Email, RealName, NickName, Password) ->
	{HashedPassword, Credential} = pre_hash:hash(Password),
	New = pre_rec_account:new(Email, RealName, NickName, HashedPassword, Credential),
	?t(pre_data:save(New)).


%% @doc Removed an account.
-spec delete(AccountID :: any()) -> {'ok'}.
delete(AccountID) ->
	?t(pre_data:delete(pre_rec_account, AccountID)).

