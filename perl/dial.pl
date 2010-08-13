#!/usr/bin/perl

# given a hash (or otherwise), find the closest writers

use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $hash = (length($ARGV[0])==40)?$ARGV[0]:sha1_hex($ARGV[0]);
printf "Dialing %s\n",$hash;

# defaults to listen on any ip and random port
my $port = 0;
my $ip = "0.0.0.0"; 
my $seed = $ARGV[1]||"telehash.org:42424";

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
$jo->{"end"} = $hash;
tsend($jo);

my %cache; # just a dumb cache of writer hashes
require "./bixor.pl"; # temp testing hack
my $buff;
$|++;
my $ipp, $ipphash;
my %resend; # sometimes need to re-send due to nat hole punching
while(1)
{
	# wait for event or timeout loop
	if(scalar $sel->can_read(5) == 0)
	{
		my $bto = bix_new($hash);
		my @ckeys = sort {bix_sbit(bix_or($bto,bix_new($a))) <=> bix_sbit(bix_or($bto,bix_new($b)))} keys %cache; # sort by closest to the hash
		print join("\n", map {$cache{$_}."\t".bix_sbit(bix_or($bto,bix_new($_)))} splice @ckeys, 0, 5),"\n";
		exit;
	}

	# must be a telex waiting for us
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");
	# TODO need some source rate detection in case there's a loop

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $writer = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "RECV[%s]\t%s\n",$writer,$buff;

	# json parse check
	my $j = $json->from_json($buff) || next;

	# discover our own ip:port
	if(!$ipp && $j->{"_to"})
	{
		printf "SELF[%s]\n",$j->{"_to"};
		$ipp = $j->{"_to"};
		$ipphash = sha1_hex($ipp);
	}

	# we've been told to talk to these writers
	if($j->{".see"})
	{
		# loop through and establish lines to them (just being dumb for now and trying everyone)
		for my $seeipp (@{$j->{".see"}})
		{
			next if($seeipp eq $ipp); # skip ourselves :)
			next if($cache{sha1_hex($seeipp)}); # skip if we know them already
			$cache{sha1_hex($seeipp)} = $seeipp;
			next if($seeipp eq $writer); # if they think they're close, they may have sent themselves

			my $jo = telex($seeipp); # send direct (should open our outgoing to them)
			$jo->{"+end"} = $hash;
			tsend($jo);

			# send nat request back to the writer who .see'd us in case the new one is behind a nat
			my $jo = telex($writer);
			$jo->{"+pop"} = "th:$seeipp";
			$jo->{"+end"} = sha1_hex($seeipp);
			tsend($jo);
		}
	}else{ # incoming telex with no .see? prolly nat hole punch, dial them again regardless
		if(!$resend{$writer})
		{
			$resend{$writer}++;
			my $jo = telex($writer);
			$jo->{"+end"} = $hash;
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

# actually send a telex to it's writer
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
