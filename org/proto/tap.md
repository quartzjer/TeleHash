Tap (.tap)
==========

The Tap is the name/value pair whereby any Switch "subscribes" to matching incoming Telexes arriving at any other Switch.

It contains a very simple filtering ruleset to indicate with Telexes it would like forwarded to it.

Lots more to define here :)

	".tap":
	[
		{"is":{ "+end":"a9993e364706816aba3e25717850c26c9cd0d89d" }, "has":[ "+foo" ] },
		{"is":{ "+foo":"0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33" } },
		{"has":[ "+foo", "+bar" ] }
	]
	