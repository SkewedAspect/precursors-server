%%% @doc Handles authentication requests. Primarily implements different hashing algorithms. If an account uses an out-
%%% dated hashing algorithm, it's this module's job to update the record when that account next authenticates.
%%%
%%% -------------------------------------------------------------------------------------------------------------------

-module(pre_hash).

% API
-export([check_password/2, hash/1, hash/2]).

-record(credential, {
	hash :: string(),
	iterations :: integer(),
	prf :: string(),
	salt :: string()
}).


% Default PBKDF2 values to the ones used by WPA2:
-define(DEFAULT_PBKDF2_ITERATIONS, 4096).
-define(DEFAULT_PBKDF2_DERIVED_LENGTH, 32).
-define(DEFAULT_PBKDF2_PRF, <<"HMAC+SHA256">>).

%% --------------------------------------------------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------------------------------------------------

%% @doc Check user credential
check_password(CheckPassword, Credential) ->
	% Example of what we (should) get from the DB:
	% {
	%     "hash": "tNcJYbTF6jJAn6mLxjAZP5Nb+yJ5P8A72YJI57rtDDU=",
	%     "iterations": 20000.0,
	%     "prf": "HMAC+SHA256",
	%     "salt": "1yfcsqA9MYJz"
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
			lager:info("Client-provided hash: ~p; stored hash: ~p", [CheckPasswordHashBase64, StoredPasswordHash]),

			pbkdf2:compare_secure(StoredPasswordHash, CheckPasswordHashBase64);

		_ ->
			lager:warning("Incompatible PRF ~p (only ~p is supported)", [PseudoRandomFunction, ?DEFAULT_PBKDF2_PRF]),
			false
	end.

%% @doc Hash passwords for authentication
hash(Password) ->
	Credential = new_credential(),
	HashedPassword = hash(Password, Credential),
	{HashedPassword, Credential}.

%% @doc Hash passwords for new accounts or authentication
hash(Password, Credential) ->
	Salt = crypto:rand_bytes(12),
	Iterations = Credential#credential.iterations,
	PseudoRandomFunction = Credential#credential.prf,
	DerivedLength = ?DEFAULT_PBKDF2_DERIVED_LENGTH,  %FIXME: Determine this from the PRF!
	case PseudoRandomFunction of
		?DEFAULT_PBKDF2_PRF ->
			%FIXME: Use the PRF!
			pbdkf2:pbkdf2(Password, Salt, Iterations, DerivedLength);
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
		prf = ?DEFAULT_PBKDF2_PRF
	}.
