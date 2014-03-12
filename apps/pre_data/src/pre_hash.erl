%%% @doc Handles authentication requests. Primarily implements different hashing algorithms. If an account uses an out-
%%% dated hashing algorithm, it's this module's job to update the record when that account next authenticates.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_hash).
-include_lib("eunit/include/eunit.hrl").

% API
-export([hash/1, hash/2]).

-record(credential, {
	iterations :: integer(),
	prf :: string(),
	salt :: string()
}).


% Default PBKDF2 values to the ones used by WPA2:
-define(DEFAULT_PBKDF2_ITERATIONS, 4096).
-define(DEFAULT_PBKDF2_DERIVED_LENGTH, 32).
-define(DEFAULT_PBKDF2_PRF, {hmac, sha256}).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Hash passwords for authentication
hash(Password) ->
	Credential = new_credential(),
	{HashedPassword, Credential1} = hash(Password, Credential),
	{HashedPassword, Credential1}.

%% @doc Hash passwords for new accounts or authentication
hash(Password, Credential) ->
	Iterations = Credential#credential.iterations,
	PseudoRandomFunction = Credential#credential.prf,
	Salt = Credential#credential.salt,
	Credential2 = Credential#credential{salt = Salt, prf = PseudoRandomFunction},
	DerivedLength = ?DEFAULT_PBKDF2_DERIVED_LENGTH,  %FIXME: Determine this from the PRF!
	case PseudoRandomFunction of
		?DEFAULT_PBKDF2_PRF ->
			%FIXME: Use the PRF!
			{ok, Hash} = pbkdf2:pbkdf2(?DEFAULT_PBKDF2_PRF, Password, Salt, Iterations, DerivedLength),
			{Hash, Credential2};
		_ ->
			lager:warning("Incompatible PRF ~p (only ~p is supported)", [PseudoRandomFunction, ?DEFAULT_PBKDF2_PRF]),
			false
	end.

%% -----------------------------------------------------------------------
%% Internal
%% -----------------------------------------------------------------------

%% @hidden
new_credential() ->
	#credential{
		iterations = ?DEFAULT_PBKDF2_ITERATIONS,
		prf = ?DEFAULT_PBKDF2_PRF,
		salt = crypto:rand_bytes(12)
	}.
