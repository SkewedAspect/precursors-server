The Plan for Entities (TM)
==========================


`pre_entity`
------------

Reads/writes from entity record in mnesia.


`entity_controller`
----------------------

A callback module associated with each entity, which defines the behavior of the entity. It can handle incoming events
and/or simulation ticks.

- Entity's state is passed into both `handle_event` and `tick`, and should be returned (possibly modified) from both.


`pre_entity_manager`
--------------------

Handles communication between the client and an entity, and between two entities.

- Calls into `pre_entity` to read or update the entity's state.


`pre_entity_simulator`
----------------------

Handles temporal simulation, calling out to each entity's controller once each tick.

- Calls into `pre_entity` to read or update the entity's state.


`pre_character`
---------------

- Handles character-level (and possibly account-level) requests from the client.
- Loads character's entities from cold storage and passes them off to `pre_entity_manager` as needed.
