#!/usr/bin/perl

# send lines from STDIN to a wall
my $wall = $ARGV[0]||"42";
my $onemsg = $ARGV[1];
my $seed = $ARGV[2]||"telehash.org:42424";

use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use Time::HiRes qw(time);
use JSON::DWIW;
my $json = JSON::DWIW->new;

# defaults to listen on any ip and random port
my $port = 0;
my $ip = "0.0.0.0"; 
my $end = sha1_hex($wall);
my $oneguid = sprintf("%f",time()); # microsecond

$iaddr = gethostbyname($ip);
$proto = getprotobyname('udp');
$paddr = sockaddr_in($port, $iaddr);
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)   or die "socket: $!";
bind(SOCKET, $paddr)                          or die "bind: $!";
$sel = IO::Select->new();
$sel->add(\*SOCKET);

# resolve our seed to it's ip:port
my($seedhost,$seedport) = split(":",$seed);
my $seedip = gethostbyname($seedhost);
my $seedipp = sprintf("%s:%d",inet_ntoa($seedip),$seedport);

# send a hello to the seed
my $jo = telex($seedipp);
$jo->{"+end"} = $end;
if($onemsg)
{
	$jo->{"+wall"} = $onemsg;
	$jo->{"+guid"} = $oneguid;
}			
tsend($jo);

my %cache; # just a dumb cache of switch hashes
my $buff;
$|++;
my $ipp, $ipphash;
my %resend; # sometimes need to re-send due to nat hole punching
while(1)
{
	# wait for event or timeout loop
	if(scalar $sel->can_read(3) == 0)
	{
		my @ckeys = sort {hash_distance($end,$a)<=>hash_distance($end,$b)} keys %cache; # sort by closest to the hash
		if(scalar @ckeys > 0)
		{
			my $target = $cache{$ckeys[0]};
			printf "attached to %s at distance %d\n",$target,hash_distance($end,$ckeys[0]);
			if(!$onemsg)
			{
				while(<STDIN>)
				{
					chop;
					my $msg = sprintf("%s",$_); # ensure it's a string
					# send to the top 3 closest
					for(my $i=0;$ckeys[$i] && $i < 3;$i++)
					{
						my $target = $cache{$ckeys[$i]};
						my $jo = telex($target);
						$jo->{"+end"} = $end;
						$jo->{"+wall"} = $msg;
						$jo->{"+guid"} = sprintf("%f",time()); # microsecond
						$jo->{"_hop"} = 1; # hop of one implies no .see reply needed here, we're too dumb to handle them
						tsend($jo);
					}
				}
			}
		}else{
			printf "no network?\n";
		}
		print join("\n", map {$cache{$_}."\t".hash_distance($end,$_)} @ckeys),"\n";
		exit;
	}

	# must be a telex waiting for us
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");
	# TODO need some source rate detection in case there's a loop

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $remoteipp = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "RECV[%s]\t%s\n",$remoteipp,$buff;

	# json parse check
	my $j = $json->from_json($buff) || next;

	# discover our own ip:port
	if(!$ipp && $j->{"_to"})
	{
		printf "SELF[%s]\n",$j->{"_to"};
		$ipp = $j->{"_to"};
		$ipphash = sha1_hex($ipp);
	}

	# we've been told to talk to these switches
	if($j->{".see"})
	{
		# loop through and establish lines to them (just being dumb for now and trying everyone)
		for my $seeipp (@{$j->{".see"}})
		{
			next if($seeipp eq $ipp); # skip ourselves :)
			next if($cache{sha1_hex($seeipp)}); # skip if we know them already
			$cache{sha1_hex($seeipp)} = $seeipp;
			next if($seeipp eq $remoteipp); # if they think they're close, they may have sent themselves

			my $jo = telex($seeipp); # send direct (should open our outgoing to them)
			$jo->{"+end"} = $end;
			if($onemsg)
			{
				$jo->{"+wall"} = $onemsg;
				$jo->{"+guid"} = $oneguid;
			}			
			tsend($jo);

			# send nat request back to the switch who .see'd us in case the new one is behind a nat
			my $jo = telex($remoteipp);
			$jo->{"+pop"} = "th:$seeipp";
			$jo->{"+end"} = sha1_hex($seeipp);
			tsend($jo);
		}
	}else{ # incoming telex with no .see? prolly nat hole punch, dial them again regardless
		if(!$resend{$remoteipp})
		{
			$resend{$remoteipp}++;
			my $jo = telex($remoteipp);
			$jo->{"+end"} = $end;
			if($onemsg)
			{
				$jo->{"+wall"} = $onemsg;
				$jo->{"+guid"} = $oneguid;
			}			
			tsend($jo);
		}
	}
	
}

# create a new telex
sub telex
{
	my $to = shift;
	my $js = shift || {};
	$js->{"_to"} = $to;
	return $js;
}

# actually send a telex to it's IPP
sub tsend
{
	my $j = shift;
	my($ip,$port) = split(":",$j->{"_to"});
	my $wip = gethostbyname($ip);
	my $waddr = sockaddr_in($port,$wip);
	my $js = $json->to_json($j);
	printf "SEND[%s]\t%s\n",$j->{"_to"},$js;
	defined(send(SOCKET, $js, 0, $waddr))    or die "send $to: $!";	
}

# returns xor distance between two sha1 hex hashes, 159 is furthest bit, 0 is closest bit, -1 is same hash
sub hash_distance
{
	my @a = map {hex $_} split undef,shift;
	my @b = map {hex $_} split undef,shift;
	my @sbtab = (-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3); # ln(hex)/log(2)
	my $ret = 156;
	for my $i (0..39)
	{
		my $byte = $a[$i] ^ $b[$i];
		return $ret + $sbtab[$byte] if($byte);
		$ret -= 4;
	}
	return -1; # same hashes?!
}