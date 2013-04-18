#!/usr/bin/env python2

import base64
import sys

import rkit
from django.utils import crypto

from models import Game, Entity, ItemDefinition, Item, Class, Power, Character, Credential, Account, Subscription

rkit.connect("localhost", int(sys.argv[1]) if len(sys.argv) > 1 else 8081)
print "Making test models..."

precursors = Game()
precursors.pk = "precursors"
precursors.name = "Requiem for Innocence: Precursors"
precursors.save()

print "Finished Game."


soldier = Class()
soldier.game = precursors
soldier.name = "Soldier"
soldier.primary_stat = "strength"
soldier.save()

operative = Class()
operative.game = precursors
operative.name = "Operative"
operative.primary_stat = "dexterity"
operative.save()

officer = Class()
officer.game = precursors
officer.name = "Officer"
officer.primary_stat = "charisma"
officer.save()

mechanic = Class()
mechanic.game = precursors
mechanic.name = "Mechanic"
mechanic.primary_stat = "intelligence"
mechanic.save()

mercenary = Class()
mercenary.game = precursors
mercenary.name = "Mercenary"
mercenary.primary_stat = "constitution"
mercenary.save()

hacker = Class()
hacker.game = precursors
hacker.name = "Hacker"
hacker.primary_stat = "intelligence"
hacker.save()

freelancer = Class()
freelancer.game = precursors
freelancer.name = "Freelancer"
freelancer.primary_stat = "charisma"
freelancer.save()

print "Finished Making Classes"


whitelynx = Account()
whitelynx.email = "david.bronke@g33xnexus.com"
whitelynx.real_name = "David H. Bronke"
whitelynx.nickname = "whitelynx"
whitelynx.save()

morgul = Account()
morgul.email = "chris.case@g33xnexus.com"
morgul.real_name = "Christopher S. Case"
morgul.nickname = "morgul"
morgul.save()

burst = Account()
burst.email = "travis.odom@g33xnexus.com"
burst.real_name = "Travis A. Odom"
burst.nickname = "Burstaholic"
burst.save()

lordnull = Account()
lordnull.email = "micahw@lordnull.com"
lordnull.real_name = "Micah Warren"
lordnull.nickname = "lordnull"
lordnull.save()

print "Finished Account."


liz = Character()
liz.account = morgul
liz.game = precursors
liz.first_name = "Lizbeth"
liz.middle_name = "Anne"
liz.last_name = "Locke"
liz.save()

simon = Character()
simon.account = morgul
simon.game = precursors
simon.first_name = "Jonathan"
simon.middle_name = "Robert"
simon.last_name = "Simon"
simon.save()

louis = Character()
louis.account = whitelynx
louis.game = precursors
louis.first_name = "Louis"
louis.last_name = "Capelle"
louis.save()

karon = Character()
karon.account = burst
karon.game = precursors
karon.first_name = "Karon"
karon.middle_name = ""
karon.last_name = ""
karon.save()

gerald = Character()
gerald.account = lordnull
gerald.game = precursors
gerald.first_name = "Gerald"
gerald.middle_name = ""
gerald.last_name = ""
gerald.save()

print "Finished Characters."


def makeCred(account, password):
    cred = Credential()
    cred.prf = "HMAC+SHA256"
    cred.salt = crypto.get_random_string()
    cred.iterations = 20000
    cred.hash = base64.b64encode(crypto.pbkdf2(password, cred.salt, iterations=cred.iterations))
    cred.account = account
    return cred

makeCred(whitelynx, "pass").save()
makeCred(morgul, "pass").save()
makeCred(burst, "pass").save()
makeCred(lordnull, "pass").save()

print "Finished Credentials."


sub = Subscription()
sub.game = precursors
sub.account = whitelynx
sub.save()

sub = Subscription()
sub.game = precursors
sub.account = morgul
sub.save()

sub = Subscription()
sub.game = precursors
sub.account = burst
sub.save()

sub = Subscription()
sub.game = precursors
sub.account = lordnull
sub.save()

print "Finished Subscription."

print "Finished Making Model Instances."
