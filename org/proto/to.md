To (_to)
========

When sending a Telex to any Switch it is highly recommended to send this name/value pair, where the value is the IPP of the recipient.

Any Switch that is receiving packets behind a NAT will not know it's public IP:PORT until it receives this, and also needs to be able to detect if it changes.