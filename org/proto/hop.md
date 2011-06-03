Hop (_hop)
==========

The hop is a name/value pair sent with any Telex to indicate if it's been forwarded and how many times.  Any Telex with an incoming hop of 4 or more should not be forwarded again (TeleHash isn't a relay network, but needs a couple hops to flexibly allow working around network oddities).

Any telex containing an end and a hop > 0 should also not be responded to with a see, as it was either a forwarded telex or the sender is indicating it doesn't want a response.