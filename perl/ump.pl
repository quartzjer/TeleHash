#!/usr/bin/perl

# Unlock My Port
# as a server, generate a random password and listen at a derivative hash, when someone wants to connect, challenge them
# as a client take given password and look for server, when found answer challenge
# +pop a port and run udptunnel on it
# include a signal that "signs" the sender IP:PORT using the pw for server/client to validate pre-challenge too
# requires http://code.google.com/p/udptunnel/

# 1. take password from args or generate one
# 2. hash it 1k times, start up switch.pl with that end and tap
# 3. when a sig comes in, verify it
# 4. +pop and try to connect back to it if good
# 5. once connected, send our own verification and a challenge
# 6. if challenge good, open another port and get its public IP:PORT, +pop that back to the requestor
# 7. once other port open/handshake, open udptunnel on it

die("temp hack, plz run from the same dir as ./switch.pl and ./udptunnel") if(!-f "switch.pl" || !-f "udptunnel");
my $wall = $ARGV[0]||"42";
my $doit = $ARGV[1];
my $end = sha1_hex($wall);
my $tap = "[{\"is\":{\"+end\":\"$end\"},\"has\":[\"+wall\"]}]";
my %dedup;

use Digest::SHA1 qw(sha1_hex);
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $run = sprintf("./switch.pl -e '%s' -t '%s' |",$end,$tap);
printf "running %s\n",$run;
open(SWITCH,$run);
while(<SWITCH>)
{
	chop;
	my $js = $json->from_json($_);
	my $wall = $js->{"+wall"};
	next if($js->{"+guid"} && $dedup{$js->{"+guid"}}++);
	if($doit)
	{
		open(R,"|$doit");
		print R $wall."\n";
		close(R);
	}
	printf "%s\t%s\n",`date`,$wall;		
}
close(SWITCH);
