Data
====


Game
----

**TODO:** Game could probably be implemented as a secondary index instead, if we don't need any other fields in the contents.

### Key
slug

### Contents
- name


Entity
------

### Key
id

### Secondary Indices
- type (enum: character, ship, other)
- name

### Contents
- display
    - model
    - materials
- state
    - position

### Links
- game (=> Game)
- account (=> Account)
- for characters:
    - possession (list of Entity)
    - inventory (list of Entity)
- for ships:
    - inventory (list of Entity)
    - system (list of ?)


Credentials
-----------

### Key
id

### Contents
- system
- for `local` credentials:
    - password salt
    - password hash
    - password hash method


Subscription
------------

### Key
id

### Links
- game (=> Game)

### Contents
- type (enum: unbilled, timespan, hours)
- ...


Account
-------

### Key
email address

### Secondary Indices
- real name
- nickname

### Links
- credentials (list of Credentials)
- character (list of Entity)
- subscription (list of Subscription)



Requests
========


Game
----

### Creating

	curl -v -X PUT -d '{"name":"Requiem for Innocence: Precursors"}' -H "Content-Type: application/json" -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" 'http://127.0.0.1:8091/riak/game/precursors?returnbody=true'


Entity
------

### Creating

	curl -v -X POST -d '{
	    "display": {
			"model": "Ships/ares",
			"materials": [
				"TODO"
			],
		},
	    "state": {
			"position": [0, 1, 2]
		}
	}' -H "Content-Type: application/json" -H "x-riak-index-type_bin: character" -H "x-riak-index-name_bin: Louis" -H 'Link: </riak/game/precursors>; riaktag="game", </riak/account/david.bronke@g33xnexus.com>; riaktag="account"' 'http://127.0.0.1:8091/riak/entity?returnbody=true'


Credentials
-----------

### Creating

	curl -v -X POST -d '{
	    "system": "local",
	    "password salt": "6LgV4/dpGg+ahvML0Y9tD01IJt4mwT/T1a+kJ23qxfsMBsQlBO9w83fHAGS1LlS7rKxj9TYsi2M5V3bOeSN/4Q==",
	    "password hash": "lbMKKz4UjdfniHFF23xjEH/0kPxmPRBwzFYMbSbO/m7q9ZwNor+LqwkrkKz2fvnT9OvN9Lqms0az8iwaSVwFhg==",
		"password hash method": "sha512"
	}' -H "Content-Type: application/json" 'http://127.0.0.1:8091/riak/credentials?returnbody=true'

For `local` credentials, see `scripts/pwhash.py` for code to generate a password hash and salt.


Subscription
------------

### Creating

	curl -v -X POST -d '{
	    "type": "unbilled",
	    "expires": null
	}' -H "Content-Type: application/json" -H 'Link: </riak/game/precursors>; riaktag="game", </riak/account/david.bronke@g33xnexus.com>; riaktag="account"' 'http://127.0.0.1:8091/riak/subscription?returnbody=true'


Account
-------

### Creating

	curl -v -X PUT -d '{}' -H "x-riak-index-real-name_bin: David H. Bronke" -H "x-riak-index-nickname_bin: whitelynx" -H "Content-Type: application/json" -H 'Link: </riak/credentials/1Bg0F72rzvhKtD7jNd5hkZcbceN>; riaktag="credentials", </riak/entity/7BcQAqy0qjcOcXEVDqJnPpWzDs3>; riaktag="character", </riak/subscription/BVia4o68VnqGhiwG9ziBmcSmbYk>; riaktag="subscription"' -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" 'http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com?returnbody=true'

### Querying

Get the account with the given nickname:

	curl -v http://127.0.0.1:8091/buckets/account/index/nickname_bin/whitelynx

Get the credentials for an account:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,credentials,_

Get the subscriptions for an account:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,subscription,_

Get the characters for an account:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,character,_

Get all games the given account is either subscribed to or has characters in:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,_,_/_,game,_
