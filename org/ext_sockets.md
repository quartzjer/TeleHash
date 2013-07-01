# `sock` - Socket Proxy

A stream of "type":"sock" is a socket proxy request for the stream to become a simple raw TCP socket proxy. The value is set to any "IP:PORT" and the receiving hashname should carefully decide if it wants to support the destination IP to prevent accidental abuse.  If the socket couldn't open or is closed at any point by either side an `end` of true is sent and any informative `err` string value can be included.

As soon as a `sock` is sent it is considered open by the sender until it receives an `end` telling it otherwise, the stream will inherently only send so much data without any confirmation from the recipient that it's open/reading.

When a new `sock` is accepted by a switch, it then opens a traditional TCP connection to the given IP:PORT value and streams all data between it and the stream, including any errors connecting and when it closes.