Kademlia
========

TeleHash builds on the Kademlia whitepaper but instead of being a Key/Value store DHT, it is a simple Pub/Sub DHT.

Instead of storing values on nodes in the DHT, TeleHash is a discovery mechanism whereby anyone can actively listen for incoming requests and respond to them by negotiating a direct P2P connection.
