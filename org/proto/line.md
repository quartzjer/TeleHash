Line (_line)
============

A Line is an active relationship between any two Switches that have exchanged Ring secrets and trust the IPP identity of the other.

The name/value pair is sent with all Telexes and required to create a Tap as well as maintain priority within Buckets.  The value is always the product of both sides Ring values.

	line = ringA * ringB

The line isn't intended to be strong security by any means, it is simply a private session identifier between any two Switches to prevent simple spoofing.  When an invalid or missing line value shows up from a known IPP that telex should be ignored, and if no other traffic comes in from the previous line value it will expire, whence a new ring handshake can happen and new line value be established.


