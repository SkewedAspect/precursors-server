-module(pre_rec_account).

-compile([{parse_transform, rec2json}]).

-record(pre_rec_account, {
	id :: any(), %auto generated id
	email :: string(), % der email, secondary index
	real_name :: string(), % optional
	nickname :: string(), % unique constraint
	password :: string(),
	hash_data :: any(), % the hash algorthm used and any args needed.
	created,
	updated
}).

