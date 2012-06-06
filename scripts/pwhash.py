#!/usr/bin/env python2

import datetime
import hashlib
import hmac
import os

HASH = lambda text: hashlib.sha512(text).digest()
SALT_BYTES = 64
ITERATIONS = 2 ** 17
CHALLENGE_BYTES = 2 ** 8


def genHash(password, salt=None):
    """Generate a random salt and a hash for the given password.

    Returns (salt, hash).

    """
    # Generate salt. <https://secure.wikimedia.org/wikipedia/en/wiki/Salt_(cryptography)>
    if salt is None:
        salt = os.urandom(SALT_BYTES)

    salt_split = int(SALT_BYTES / 2)

    # Perform key stretching. <https://secure.wikimedia.org/wikipedia/en/wiki/Key_stretching>
    key = ""
    for i in range(ITERATIONS):
        # Split the salt in half and add it to the beginning and end, and inject the current iteration in between the
        # accumulated key and the password. This may or may not actually add any security; this has not been tested nor
        # proven. It is based on a hunch that it may help defend against attacks similar to those described in the
        # design principles of HMAC: <https://secure.wikimedia.org/wikipedia/en/wiki/HMAC#Design_principles>
        key = HASH(salt[:salt_split] + key + str(i) + password + salt[salt_split:])

    return (salt, key)


def genChallenge():
    return os.urandom(CHALLENGE_BYTES)


def genChallengeResponse(challenge, passwordHash):
    return hmac.new(passwordHash, challenge, hashlib.sha512).digest()


def checkChallengeResponse(challenge, response, passwordHash):
    return response == genChallengeResponse(challenge, passwordHash)


if __name__ == '__main__':
    from base64 import b64encode, b64decode
    from getpass import getpass

    action = "password hash"
    #action = "iterations range"
    #action = "authentication"

    if action == "password hash":
        print "Generating password hash and salt..."

        password = getpass()
        print "Creating hash using {} iterations...".format(ITERATIONS)

        start = datetime.datetime.now()
        passwordSalt, passwordHash = genHash(password)
        end = datetime.datetime.now()
        print "Hash generated in", end - start

        # Base64-encode the output.
        print "Salt:", b64encode(passwordSalt)
        print "Hash:", b64encode(passwordHash)

    elif action == "iterations range":
        print "Generating password hashes using (2 ** 16) through (2 ** 24) iterations..."

        password = getpass()

        for iterExponent in range(16, 25):
            ITERATIONS = 2 ** iterExponent
            print "Creating hash using {} (2 ** {}) iterations...".format(ITERATIONS, iterExponent)

            start = datetime.datetime.now()
            passwordSalt, passwordHash = genHash(password)
            end = datetime.datetime.now()
            print "Hash generated in", end - start

            # Base64-encode the output.
            print "Salt:", b64encode(passwordSalt)
            print "Hash:", b64encode(passwordHash)

    elif action == "authentication":
        print "Testing the authentication process..."

        class Server(object):
            def __init__(self):
                print "server started; requesting password."
                self.passwordSalt, self.passwordHash = genHash(getpass())

            def startAuthentication(self, client):
                print "client => server | started authentication"
                self.client = client

                self.challenge = genChallenge()

                self.client.challenge(b64encode(self.challenge), b64encode(self.passwordSalt))

            def respond(self, response):
                print "server <= client | response:", response

                expected = b64encode(genChallengeResponse(self.challenge, self.passwordHash))
                print "server | expected response:", expected

                succeeded = checkChallengeResponse(self.challenge, b64decode(response), self.passwordHash)
                self.client.authReply(succeeded)

        class Client(object):
            def connect(self, server):
                print "client | connecting to server"
                self.server = server
                self.server.startAuthentication(self)

            def challenge(self, challenge, passwordSalt):
                print "server => client | challenge:", challenge, "| passwordSalt:", passwordSalt
                challenge = b64decode(challenge)
                passwordSalt = b64decode(passwordSalt)

                passwordSalt, passwordHash = genHash(getpass(), passwordSalt)

                response = b64encode(genChallengeResponse(challenge, passwordHash))
                self.server.respond(response)

            def authReply(self, succeeded):
                print "server => client | auth succeeded?", succeeded

        server = Server()
        client = Client()
        client.connect(server)
