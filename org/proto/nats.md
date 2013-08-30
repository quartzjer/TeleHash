NATs (Network Address Translators)
==================================

In order two work around the existence of NATs when establishing connections between any two Switches, extra work must be done to negotiate mappings through them.

Any Switch that detects (detection is when the IPP the OS tells you differs from the incoming to values being received from remote switches) it's behind a NAT should also Tap itself with a Pop to any Switches is has an active Line with.  Requesting Switches can then send a Pop request and negotiate a direct connection (poke a hole in one or both NATs). 

A is behind a NAT and sends B a tap of itself (the end is A's IPP)

	{
		".tap":[{"is":{ "+end":"a9993e364706816aba3e25717850c26c9cd0d89d" },"has":["+pop"]}]
	}

C is behind a NAT and discovered A from B, and does two things either in parallel or sequentially if the first fails:

C->A (in case A isn't behind a NAT and it can go direct)
	{
		"+end":"a9993e364706816aba3e25717850c26c9cd0d89d"
	}
C->B (as a secondary path)
	{
		"+end":"a9993e364706816aba3e25717850c26c9cd0d89d",
		"+pop":"th:CIP:CPORT"
	}
B->A (just forwarding due to the tap)
{
	"+end":"a9993e364706816aba3e25717850c26c9cd0d89d",
	"+pop":"th:CIP:CPORT",
	"_hop":1
}

A then knows to send a tiny hello Telex to C to open up a mapping from its NAT to C:
	{
		"_to":"CIP:CPORT"
	}

Now C will have gotten that one since it had sent A a telex to open the mapping through its NAT, and it can send any subsequent Telex it wants (either an end it's dialing, or a tap, or negotiate a direct connection for an app, etc).

Switches can also negotiate internal network direct connections when they detect a peer they're trying to connect to is behind the same NAT (since the peer has the same IP but a different PORT).  Not defined here (yet) would be using Diffie–Hellman involving an external switch to swap each other's internal IP (10.x 172.x 192.x etc) to connect internally, without revealing that IP to the public network.


