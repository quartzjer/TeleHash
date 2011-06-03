Kademlia
========

TeleHash builds on the Kademlia whitepaper but instead of being a Key/Value store DHT, it is a simple Pub/Sub DHT.

Instead of storing values on nodes in the DHT, TeleHash is a discovery mechanism whereby anyone can actively listen for incoming requests and respond to them by negotiating a direct P2P connection.

Most of the primitives described in Kademlia work identically, the buckets, the lookups (end/see), the multiple-requests-in-parallel, and the necessity to ping to keep NATs mappings open and track vitality of peers.  The largest difference is that instead of storing or getting a key/value, in TeleHash you tap (subscribe/listen) and dial (publish/announce).
