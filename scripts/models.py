import rkit
from rkit import models

#TODO: Move out into some other file. Settings?
#rkit.connect("localhost", 8081)

class Game(models.Model):
    name = models.StringField(required=True)


class Entity(models.Model):
    type = models.StringField(choices=["ship", "other"], default="other")
    name = models.StringField(required=True)
    game = models.LinkField(related_model=Game, related_name="entities", required=True)


class ItemDefinition(models.Model):
    class Meta:
        bucket_name = "item_def"

    name = models.StringField(required=True)
    game = models.LinkField(related_model=Game, related_name="item_defs", required=True)


class Item(models.Model):
    definition = models.LinkField(related_model=ItemDefinition, related_name="instances", required=True)


class Class(models.Model):
    name = models.StringField(required=True, index=True)
    primary_stat = models.StringField(choices=["strength", "dexterity", "constitution", "intelligence", "charisma", "willpower"], required=True)
    game = models.LinkField(related_model=Game, related_name="classes", required=True)


class Power(models.Model):
    name = models.StringField(required=True)
    class_ = models.LinkField(name="class", related_model=Class, related_name="powers")
    game = models.LinkField(related_model=Game, related_name="powers", required=True)


class Account(models.Model):
    email = models.StringField(required=True, primary=True)
    real_name = models.StringField(index=True)
    nickname = models.StringField(index=True)


class Credential(models.Model):
    type = models.StringField(choices=["local"], default="local", required=True)

    prf = models.StringField(required=True)
    hash = models.StringField()
    salt = models.StringField()
    iterations = models.NumberField()

    account = models.LinkField(related_model=Account, related_name="credentials")


class Character(models.Model):
    account = models.LinkField(related_model=Account, related_name="characters", required=True)
    game = models.LinkField(related_model=Game, related_name="characters", required=True)

    first_name = models.StringField(required=True)
    middle_name = models.StringField()
    last_name = models.StringField(required=True)
    nickname = models.StringField()

    # Character stuff
    strength = models.NumberField(required=True, default=1.0)
    dexterity = models.NumberField(required=True, default=1.0)
    constitution = models.NumberField(required=True, default=1.0)
    intelligence = models.NumberField(required=True, default=1.0)
    charisma = models.NumberField(required=True, default=1.0)
    willpower = models.NumberField(required=True, default=1.0)

    level = models.NumberField(required=True, default=1)
    experience = models.NumberField(required=True, default=0)
    class_ = models.LinkField(name="class", related_model=Class, related_name="characters", required=True)

    # Ships, cars, etc.
    possession = models.LinkCollection(related_model=Entity)

    # Guns, armor, etc.
    inventory = models.LinkCollection(related_model=Item)

    def full_name(self):
        return "{} {} {}".format(self.first_name, self.middle_name, self.last_name)


class Subscription(models.Model):
    type = models.StringField(choices=["unbilled", "timespan", "hours"], default="unbilled", required=True)
    expires = models.StringField(null=True)
    game = models.LinkField(related_model=Game, related_name="subscriptions", required=True)
    account = models.LinkField(related_model=Account, related_name="subscriptions", required=True)
