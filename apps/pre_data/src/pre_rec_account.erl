-module(pre_rec_account).

-compile([{parse_transform, rec2json}]).

-export([new/5]).

-record(pre_rec_account, {
	id :: any(), %auto generated id
	email :: binary(), % der email, secondary index
	real_name :: binary(), % optional
	nickname :: binary(), % unique constraint
	password :: binary(), % hash of the password
	hash_data :: any(), % the hash algorthm used and any args needed.
	created,
	updated
}).

new(Email, RealName, NickName, Password, Credential) ->
	#pre_rec_account{
		email = Email,
		real_name = RealName,
		nickname = NickName,
		password = Password,
		hash_data = Credential
	}.

