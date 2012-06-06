Data
====


Game
----
key: slug

- name


Credentials
-----------
key: id

- system
- for `local` credentials:
    - password salt
    - password hash


Subscription
------------
key: id

- game
- type (enum: unbilled, timespan, hours)
- ...


Account
-------
key: email address

- real name
- nickname
- credentials (list)
- characters (list)
- subscriptions (list)


Entity
------
key: id

- game
- type (enum: character, ship, other)
- name
- display
    - model
    - materials
- state
- for characters:
    - possessions (list of Entity)
    - inventory (list of Entity)
- for ships:
    - inventory (list of Entity)
    - systems (list of ?)



Requests
========


Game
----

### Creating

	curl -v -X PUT -d '{"name":"Requiem for Innocence: Precursors"}' -H "Content-Type: application/json" -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" 'http://127.0.0.1:8091/riak/game/precursors?returnbody=true'


Credentials
-----------

### Creating

	curl -v -X POST -d '{
	    "system": "local",
	    "password salt": "6LgV4/dpGg+ahvML0Y9tD01IJt4mwT/T1a+kJ23qxfsMBsQlBO9w83fHAGS1LlS7rKxj9TYsi2M5V3bOeSN/4Q==",
	    "password hash": "lbMKKz4UjdfniHFF23xjEH/0kPxmPRBwzFYMbSbO/m7q9ZwNor+LqwkrkKz2fvnT9OvN9Lqms0az8iwaSVwFhg=="
	}' -H "Content-Type: application/json" -H 'Link: </riak/account/david.bronke@g33xnexus.com>; riaktag="account"' 'http://127.0.0.1:8091/riak/credentials?returnbody=true'

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

	curl -v -X PUT -d '{
	    "real name": "David H. Bronke",
	    "nickname": "whitelynx",
	}' -H "Content-Type: application/json" -H 'Link: </riak/credentials/K9Nn3jNbZnYidFm85GeaQqljTkV>; riaktag="credentials", </riak/subscription/4g54q1lCzHtEOVEuwYFCNZv10W6>; riaktag="subscription"' -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" 'http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com?returnbody=true'

### Querying

Get the credentials for an account:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,credentials,_

Get the subscriptions for an account:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,subscription,_

Get the characters for an account:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,character,_

Get all games the given account is either subscribed to or has characters in:

	curl -v http://127.0.0.1:8091/riak/account/david.bronke@g33xnexus.com/_,_,_/_,game,_
