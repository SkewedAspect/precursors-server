import os
import hashlib
from getpass import getpass
from pymongo import Connection
#------------------------------------------------------------------------------#

account = {'characters': []}

#------------------------------------------------------------------------------#

connection = Connection('localhost', 27017)
server_db = connection.precursors_server

#------------------------------------------------------------------------------#

print "Welcome to the Precursors Account Creation Script.\n"

account['name'] = raw_input("Please enter your username: ")
password = getpass("Please enter your password: ")
account['email'] = raw_input("Please enter your email: ")
account['real_name'] = raw_input("Please enter your full name: ")

#------------------------------------------------------------------------------#

print "\nGenerating account..."

# Generate a 16 digit, random salt
account['password_salt'] = "".join(map(lambda x: "%x" % ord(x), os.urandom(16)))

# Generate password hash
account['password_hash'] = hashlib.sha256(password + account['password_salt']).hexdigest()

#------------------------------------------------------------------------------#

# Insert the dictionary as a document
server_db.accounts.insert(account)

print "\nAccount created successfully!"

#------------------------------------------------------------------------------#

