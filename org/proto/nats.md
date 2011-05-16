NATs (Network Address Translators)
==================================

In order two work around the existence of NATs when establishing connections between any two Switches, extra work must be done to negotiate mappings through them.

Any Switch that detects (detection to be defined) it's behind a NAT should also Tap itself with a Pop to any Switches is has an active Line with.  Requesting Switches can then send a Pop request and negotiate a direct connection (poke a hole in one or both NATs). 

(Also note how to negotiate internal network direct connections when multiple Switches are behind the same NAT)