Bytes Received (_br)
====================

This is a name/value pair sent with any Telex to indicate to the recipient how many bytes have been received from them at that point.  The value is always a positive integer.

It is a very minimal network diagnostic that is useful to detect packet loss and latency to any other switch, just track all outgoing bytes sent to another switch and compare it to the received br values, once they differ too much (~10k is a good start under normal circumstances) then the line might need to be dropped.

The difference between sent and received can be used as an indicator of quality in determining preference/order/persistence in buckets.  The time taken between sent and received confirmation could also be used to track quality.  Either of these might prevent a peer switch from being advertised in see responses.

The br can also be used as a way to artificially negatively signal to another Switch to lower priority or prevent it from being announced.

Any sending switch should never send more than 10k without having gotten a response br receipt back either, this controls flooding, and lets another switch send an artificially low or 0 number to slow/mute incoming packets from a peer.