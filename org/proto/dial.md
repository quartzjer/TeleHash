Dialing (DHT Lookup)
====================

The process of "dialing" is the recursive search of the DHT looking for a specific End hash.

Each switch that is sent an End should respond with a See of other closer Switches (or itself).  The dialing switch recursively asks closer and closer ones until it finds the set that are the closest.

When dialing, any switch sent an end hash will respond with a see array of new IPPs, and the fastest and most successful strategy is to subsequently ask multiples of those IPPs again with the end hash, aggregating responses and repeating in parallel.  Usually asking a few in parallel continuously is a good strategy.

Send:
	{
		"+end":"a9993e364706816aba3e25717850c26c9cd0d89d"
	}

Receive:
	{
		".see":["5.6.7.8:23456","11.22.33.44:11223"]
	}
