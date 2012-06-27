-module(pre_pwhash).

-export([genHash/2, genHash/3, genChallenge/0, genChallengeResponse/2, checkChallengeResponse/3]).

%---------------------------------------------------------------------------------------------------------------------%

-define(CHALLENGE_BYTES, math:pow(2, 8)).

-record(password_hash_info, {
	%hash_fun = fun erlsha2:sha512/1 :: function(),
	hash_fun = fun sha2:sha512/1 :: function(),
	salt_bytes = 64 :: integer(),
	iterations = math:pow(2, 17) :: integer()
}).

%=====================================================================================================================%
%% API

%---------------------------------------------------------------------------------------------------------------------%
%% @doc Generate a random salt and a hash for the given password.

-spec genHash(Password, HashInfo) -> {Salt, Hash} when
	Password :: binary(),
	HashInfo :: #password_hash_info{},
	Salt :: binary(),
	Hash :: binary().

genHash(Password, HashInfo) ->
	#password_hash_info{
		salt_bytes = SaltBytes
	} = HashInfo,

	Salt = genSalt(SaltBytes),

	{genHash(Password, HashInfo, Salt), Salt}.

%---------------------------------------------------------------------------------------------------------------------%
%% @doc Generate a hash for the given password, using the given salt.
%%
%% To test:
%%
%%   rr("src/utils/pre_pwhash.erl"), timer:tc(fun() -> base64:encode(pre_pwhash:genHash(ssh_io:read_password("Password: "), #password_hash_info{}, base64:decode(<<"6LgV4/dpGg+ahvML0Y9tD01IJt4mwT/T1a+kJ23qxfsMBsQlBO9w83fHAGS1LlS7rKxj9TYsi2M5V3bOeSN/4Q==">>))) end).

-spec genHash(Password, HashInfo, Salt) -> Hash when
	Password :: binary(),
	HashInfo :: #password_hash_info{},
	Salt :: binary(),
	Hash :: binary().

genHash(Password, HashInfo, Salt) ->
	#password_hash_info{
		hash_fun = HashFun,
		salt_bytes = SaltBytes,
		iterations = Iterations
	} = HashInfo,

    Salt2Length = round(SaltBytes / 2),
	Salt1Length = SaltBytes - Salt2Length,

	<<Salt1:Salt1Length/binary, Salt2:Salt2Length/binary>> = Salt,

	iterateHash(Password, HashFun, Salt1, Salt2, Iterations).

%---------------------------------------------------------------------------------------------------------------------%
%% @doc Generate a random challenge binary.

-spec genChallenge() -> binary().

genChallenge() ->
	crypto:strong_rand_bytes(?CHALLENGE_BYTES).
	% If the above is too slow:
	%crypto:rand_bytes(CHALLENGE_BYTES).

%---------------------------------------------------------------------------------------------------------------------%
%% @doc Generate a response for a the given challenge and password hash.

-spec genChallengeResponse(Challenge, PasswordHash) -> Response when
	Challenge :: binary(),
	PasswordHash :: binary(),
	Response :: binary().

genChallengeResponse(Challenge, PasswordHash) ->
	hmac:hmac512(PasswordHash, Challenge).

%---------------------------------------------------------------------------------------------------------------------%
%% @doc Check the validity of the given challenge response.

-spec checkChallengeResponse(Challenge, Response, PasswordHash) -> boolean() when
	Challenge :: binary(),
	PasswordHash :: binary(),
	Response :: binary().

checkChallengeResponse(Challenge, Response, PasswordHash) ->
    Response == genChallengeResponse(Challenge, PasswordHash).

%=====================================================================================================================%
%% Internal functions

%---------------------------------------------------------------------------------------------------------------------%
%% @doc Perform key stretching. <https://secure.wikimedia.org/wikipedia/en/wiki/Key_stretching>

iterateHash(Password, HashFun, Salt1, Salt2, Iterations) when is_list(Password) ->
	iterateHash(list_to_binary(Password), HashFun, Salt1, Salt2, Iterations);

iterateHash(Password, HashFun, Salt1, Salt2, Iterations) ->
	iterateHash(Password, HashFun, Salt1, Salt2, round(Iterations), 0, <<>>).

iterateHash(Password, HashFun, Salt1, Salt2, Iterations, CurrentIter, Acc) ->
	% Split the salt in half and add it to the beginning and end, and inject the current iteration in between the
	% accumulated key and the password. This may or may not actually add any security; this has not been tested nor
	% proven. It is based on a hunch that it may help defend against attacks similar to those described in the
	% design principles of HMAC: <https://secure.wikimedia.org/wikipedia/en/wiki/HMAC#Design_principles>
	CurrentIterBin = list_to_binary(integer_to_list(CurrentIter)),
	NewAcc = HashFun(<<Salt1/binary, Acc/binary, CurrentIterBin/binary, Password/binary, Salt2/binary>>),
	%NewAcc = HashFun(lists:flatten([Salt1, Acc, integer_to_list(CurrentIter), Password, Salt2])),
	%NewAcc = HashFun(Acc),

	case CurrentIter + 1 of
		Iterations ->
			NewAcc;
		_ ->
			iterateHash(Password, HashFun, Salt1, Salt2, Iterations, CurrentIter + 1, NewAcc)
	end.

%---------------------------------------------------------------------------------------------------------------------%
%% @doc Generate salt. <https://secure.wikimedia.org/wikipedia/en/wiki/Salt_(cryptography)>

genSalt(SaltBytes) ->
	crypto:strong_rand_bytes(SaltBytes).
	% If the above is too slow:
	%crypto:rand_bytes(SaltBytes).
