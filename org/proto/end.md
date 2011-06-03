End (hash)
==========

The End is simply the SHA1 hash that is the focus of any Dialing process or sending a Telex to any Switch.

Ends are usually the hash of another shared or commonly known identifier, such as a URL, email, handle, geohash, or key, etc.

	{
		"+end":"a9993e364706816aba3e25717850c26c9cd0d89d"
	}

Upon receipt Switches should always respond with a see of closer Switches (or itself if it's the closest), unless the hop count is > 0, then no response is necessary.