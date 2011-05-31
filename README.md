TeleHash - Distributed JSON - http://telehash.org/
==================================================

* org/		telehash.org contents (all docs so far)
* perl/		test implementations and utils in Perl
* c/			basic test stuff in C
* ruby/ 		rudimentary testing in Ruby
* erlang/		basic announcer in erlang
* switchd/	plans to create a utility daemon to do the dirty work
* diag/	 	graffle/diagrams of protocol states


What tech does one need to implement a switch?
==============================================

*	UDP
*	JSON
*	SHA1
*	XOR'ing the 160 bits of a SHA1 hash
*	handling both network events and timers (and possibly interactions with the "app" it's serving)

What does a switch need to do?
==============================
	- listen for UDP packets (off a random port)
	- send something to an initial seed to discover its public IP:PORT
	- announce itself and try to discover other switches nearby to it
	- maintain the "lines" it has active with any other switches
		- validate sender/status
		- track bytes sent/received
	- answer requests for nearby switches
	- accept "taps" from other switches
	- match incoming signals against any active taps and forward them
	- that's plenty :) 