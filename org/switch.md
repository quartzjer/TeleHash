switch implementation outline
=============================

This is a rough high level guide for implementing a switch.

## Core Functions

* decode -> udp binary packet into json + body
* understand -> look at the json keys on a packet and trigger an action (see Logic Flows)
* verify -> process op/open packets, verify signature

## Install a Space

* provide the name of the space
* provide list of one or more operator addresses (hashname,ip,port)

## Install a Hashname

* provide the space, public key, private key, flags
* validates with the space
* if spacer flag is set, start meshing
* optional callbacks for *unhandled* packets for this hashname
	* stream
	* line
	* op (only if operator)
	* anon

A switch should also have built-in support to be an operator or socket (and optionally http) proxy that can be enabled per hashname. To become an operator there should be a way to lookup keys for it's space (be given a callback/function to do so). To act as a socket or http proxy, provide an option to pass in a destination ip:port can enable this function.

If spacer is supported, an installed hashname can be instructed to resolve any given hash (or a string that is then sha1'd) to a list of hashnames within it's space closest to it.

Any installed hashname should support the following actions when given another hashname to connect to, the switch should do all the necessary op, line, and stream actions as required:

* proxy - given a target ip:port, return a local ip:port connected to it
* http - given a request (method, path, query, headers, body, etc) send it
* stream - given a callback, open a generic stream and return any unhandled packets from it
* line - given a callback, open a line and return any unhandled packets from it

## Primary lookup tables required

* hashname -> hashname object
* line -> hashname (for active lines)
* key -> op requests (transient)
* see -> seeking object (transient, if spacer is supported)

## Objects

### Space

* name
* list of operator hashnames
* distance index of hashnames (if spacer is supported)

### hashname

* state
* ip:port
* space
* key
* line
* streams index (only when line is active)

### hosted hashname

* it's hashname object
* optional callback for unhandled line packets
* optional callback for unhandled stream packets
* any optional proxy ip:port
* if it's an operator, a key lookup callback

### stream

* last processed seq
* last confirmed seq
* dup count
* index (by seq) of packets received
* index of packets sent
* optional app-provided callback

## Logic Flows

### parsed packet processing (understand)

Based on the keys in the JSON on the packet, trigger the following actions:

1. key - only comes as answer to an op request, look up in table
2. open - verify the sender and respond with a line if acceptable
3. line - lookup in table, if one exists attach it's hostname and continue, else drop
4. sline - same as line, decode body and use that to replace the current JSON/BODY and continue
5. seek - (if spacer is supported) if not w/ a line validate the via, optionally respond with a see
6. if no line, see if there's a global app callback for anon packets, otherwise drop

The following only happen when there's a line or stream open:

1. pop - parse and send the requested packet
2. if no stream, see if there's a generic line handler callback for the line's receiving hashname
3. stream - do stream processing
4. if the stream was opened by this hashname, see if there's a configured callback for it
3. sock - (stream required) if there's a proxy configured, do socket processing
4. req (stream required) if proxy or callback, do http proxy processing
5. anything else, see if there's a generic stream handler for the line's receiving hashname

### stream processing (minimal/default)

* incoming packet
	* if received index is > 100, drop (had 3 chances)
	* if seq was already received, dup++
	* if incoming seq - last confirmed seq is > 30, or dup count >= 5
		* send contentless packet w/ current range/miss
		* update last confirmed seq to incoming seq
		* reset dup count
	* if seq is oldest sequential, process it and last++
		* check if next seq is in received index and repeat
	* if not, put in received queue
	* any misses
		* start misses timer if not running
		* set missed=1 on outgoing index packet
	* any range (minus misses), remove from outgoing index
* outgoing packet
	* attach range+miss based on received index
	* put in outgoing index
	* if outgoing index > 100
		* set missed=1 (if not set) on whole outgoing index and start timer
		* don't send (they'll be in the index and get marked as missed eventually if alive)
* misses timer 5sec
	* look for the first 5 misses, resend, increment
	* if any misses > 3 and last received > 1min, kill stream
	* no misses, delete timer
