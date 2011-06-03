Congestion
==========

When there's a lot of attention around one particular hash or address space (when an app is using something like geohash for instance), switches nearby can become overwhelmed with requests.  This happens in two ways, either with tap requests coming in or with too many end signals flooding in.

For taps the response is easy, any switch can just set a maximum threshold for the number of taps it wants to maintain, and can get fancy in trying to balance/detect common taps to ignore (normalize and hash the tap or the rules in a tap) if it wants.  When a switch that wants to tap that area fails, it should continue trying to tap other nearby switches until it moves far enough out/away on the ring to find capacity.

For a large influx of end requests there are a number of possible strategies (that need to be explored and tested), including any given switch tracking how many times it returns a peer's IPP in a see response and never over-announcing it's presence, using br if becoming overwhelmed to announce to nearby neighbors that it's lower priority and shouldn't be announced, etc.

The goal is to KISS in handling these challenges by pushing the hot zones further out the ring, and therefore onto larger and larger swaths of switches.  Applications that need to operate around high density hashes may need to do extra work to tap larger areas as well.