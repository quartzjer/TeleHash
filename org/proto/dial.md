Dialing (DHT Lookup)
====================

The process of "dialing" is the recursive search of the DHT looking for a specific End hash.

Each switch that is sent an End should respond with a See of other closer Switches (or itself).  The dialing switch recursively asks closer and closer ones until it finds the set that are the closest.