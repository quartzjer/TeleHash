Buckets (Kademlia)
==================

The concept of a "bucket" is core to Kademlia and therefore TeleHash, there is one bucket per distance step from the local Switch to any remote one, 160 of them.

The bucket limits the number of open lines to other distant Switches and encourages maintaining closer ones.

How the priority of which Switches to keep in a bucket versus expire is managed is a key factor in dialing efficiency and network resiliency.