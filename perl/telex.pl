
# hooks for app usage


# send a telex to a writer, args being:
#	ip:port (string)
#	end (string, hash code, optional)
#	{sig=>value} (hash reference of other signals)
#	{.cmd=>value} (hash reference of any commands)
#	callback (callback reference for any response telexes)
#	expiration (for callback)
sub telex_send
{
	# check the commands to set up filters for a callback
	# look at the outgoing telexes and see if this can be merged with any or needs a new one
}
