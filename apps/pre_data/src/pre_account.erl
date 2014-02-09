%%% @doc Convenience wrapper for working with accounts. Should become more useful once we add things like player mail
%%% and other account management features. For now, it's just wrappers around `pre_data' calls.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_account).

% API
-export([get_by_email/1, get_by_id/1, create/1, delete/1]).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Gets an account by email address. Returns an account.
-spec get_by_email(EmailAddress :: binary()) -> {'ok', tuple()}.
get_by_email(_EmailAddress) ->
  {ok, {}}.

%% @doc Gets an account by id. Returns an account.
-spec get_by_id(AccountID :: binary()) -> {'ok', tuple()}.
get_by_id(_AccountID) ->
  {ok, {}}.

%% @doc Creates a new account. Returns the newly created account.
-spec create(AccountRecord :: tuple()) -> {'ok', tuple()}.
create(_AccountRecord) ->
  {ok, {}}.

%% @doc Removed an account.
-spec delete(AccountID :: binary()) -> {'ok', tuple()}.
delete(_AccountID) ->
  {ok, {}}.
