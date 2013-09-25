#DHT Implementation Guidelines

The following document lists some implementation guidelines and heuristics for
implementing a telehash Distributed HashTable. 

> Note: several heuristics (especially numerical values) in this document are
> based on other networks with different usage patterns, or completely made up.
> Expect some of these values to change as the DHT becomes large enough to have
> observable behaviors.

##DHT Behavior
The DHT is based on Kademlia, with several important differences:
 - Node identifiers are not random, but instead are hashnames, based on the
   public portion of a RSA keypair.
 - Node identifiers are 256 bits, not 160 bits
 - All communication takes place over an open line, including DHT operations.
 - Having a hashname, IP and port is insufficient to connect to a new peer. You
   must have the RSA public key of the peer in order to talk to them
 - Intermediate nodes introduce us to closer or destination nodes, and in the
   process aid in exposing a public UDP port.
 - The DHT mechanism is only used to find other hosts, and does not contain
   find_value or store commands to find, store and replicate other values.

Some guidelines:
   
 1. Peers can be sorted into five categories:
    - Unconnected peers - have no line with the peer
	- Unconnected seeds - have no line with the peer, but expectation of direct
	  connectability and no need for introduction.
	- Active Line - have had an exchange resulting in me receiving an ack in
	  the last 15 minutes.
	- Inactive Line - no qualifying exchange in the last 15 minutes
	- Bad Line - has had a number of sequential failures
 2. When receiving a "seek" command (similar to find_node in Kademlia), you
    should strongly prefer returning active line peers. 
 3. Packet retry logic is not always required. Best to leave retry logic to
    higher level concepts like finding a hashname or sending streamed data.
 4. Cache sent open packets until a line is opened. Send the same open packet on
    either side if you need to retry. This will eliminate potential issues with
	different DH keys negotiated on each side.
 5. If you suspect you are behind a stateful firewall/NAT, you should send or
    receive one packet within every minute to maintain your UDP port. It would
	be recommended to ping the line which has gone the longest without
	interaction.
 6. Keep track of ping failures, stream retries to an open line, or open
    retries against a node. After a certain number of failures, that node can
	be considered 'bad' and dropped from the DHT.
 7. It is recommended to have a reasonably long initial delay before retrying
    with an exponential falloff. Assume nodes who do not immediately respond
	are actually under some congestion or CPU load - sending more traffic
	probably won't help.
 8. When seeking for a particular hashname, it is recommended putting all
    established lines and seeked hashnames off of established lines in a
	ordered list based on distance to the destination.
 9. Rather than treating a peer/connect as something to be waited on and
    retried, pursue several of the closest nodes to the destination at once
 10. The DHT results should favor returning nodes which you have known about for
    the longest period of time. Kademlia has buckets of a maximum size sorted by
    LRU, but does not free up a bucket until one of the nodes in it is known to
    be bad.

Two ways to start using network:
1. Start as a seed: Randomly pick a seed and ask it for peers closest to me (seek myself). Establish lines with them.

   > Question: could I seek my complement node? (aka me xor 0xffff...)
2. Seeds are treated as lines for the purpose of firing off seek requests.
