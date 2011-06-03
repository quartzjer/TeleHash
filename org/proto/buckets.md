Buckets (Kademlia)
==================

The concept of a "bucket" is core to Kademlia and therefore TeleHash, there is one bucket per distance step from the local Switch to any remote one, 160 of them.

Generally, the first few buckets will always be "full" as there's always a lot of switches far away from yourself (bucket 0 is 50% of the address space, bucket 1 is 25%, etc).  Every switch should try to keep as many lines open and as many buckets as full as it can, as the more peers known the faster the lookups are, but the tradeoff is the ping activity maintenance and tracking overhead.  If there is a minimum of one packet sent (and received) from every other switch, the number of packets per second is pretty easy to baseline.

Switches can set a maximum number of peers to, for instance, 500, and divide that by the number of buckets that have at least one peer in them, for instance, 20, and then try to maintain about 25 peers per bucket.

The bucket limits constrain the number of open lines to other distant Switches and encourages maintaining closer ones.

How the priority of which Switches to keep in a bucket versus expire is managed is a key factor in dialing efficiency and network resiliency, see br for one of the inputs on this too.