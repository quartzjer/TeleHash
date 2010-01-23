#!/usr/bin/perl

# a prototype telehash writer

# Jer 1/2010

use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

# defaults to listen on any ip and random port
my $port = $ARGV[0]||0;
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
$jo->{".end"} = sha1_hex(rand()); # random end, just to provoke a .see that has a _to to identify ourselves
tsend($jo);

my %cache; # just a dumb cache of writer hashes
my %lines; # static line assignments per writer
my %ends; # any end hashes that we've handled
require "./bixor.pl"; # temp testing hack
my $buff;
$|++;
my $ipp, $ipphash;
while(1)
{
	# wait for event or timeout loop
	if(scalar $sel->can_read(60) == 0)
	{
		printf "LOOP\n";
		tscan();
		next;
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

	# if this is a writer we know, check a few things
	if($lines{$writer})
	{
		# if we've not seen them in a while, full reset!!
		$lines{$writer} = {} if(time() - $lines{$writer}->{"last"} > 600);

		# keep track of when we've last got stuff from them
		$lines{$writer}->{"last"} = time();
		
		# check to see if the line matches, and skip if not
		if($lines{$writer}->{"_line"} && $lines{$writer}->{"_line"} != $j->{"_line"})
		{
			print "LINE MISMATCH!\n";
			next;
		}
	}

	# track all seen writers for .end/.see testing
	$cache{sha1_hex($writer)}=$writer;
	
	# discover our own ip:port
	if(!$ipp && $j->{"_to"})
	{
		printf "SELF[%s]\n",$j->{"_to"};
		$ipp = $j->{"_to"};
		$ipphash = sha1_hex($ipp);
		$cache{$ipphash}=$ipp; # for .end processing, to know ourselves
	}

	# a request to find other writers near this .end hash
	if($j->{".end"})
	{
		my $bto = bix_new($j->{".end"}); # convert to format for the big xor for faster sorting
		my @ckeys = sort {bix_sbit(bix_or($bto,bix_new($a))) <=> bix_sbit(bix_or($bto,bix_new($b)))} keys %cache; # sort by closest to the .end
		printf("from %d writers, closest is %d\n",scalar @ckeys, bix_sbit(bix_or($bto,bix_new($ckeys[1]))));
		# is this .end closest to US?  If so, handle it
		if($ckeys[0] eq $ipphash)
		{
			printf("handling!\n");
		}else{ # otherwise send them back a .see of ones closer
			my @cipps = map {$cache{$_}} splice @ckeys, 0, 5; # just take top 5 closest
			my $jo = telex($writer);
			$jo->{".see"} = \@cipps;
			tsend($jo);			
		}
		doend($j,$writer); # could always be registered forwards
	}

	# a request to send a .nat to a writer that we should know (and only from writers we have a line to)
	if($j->{".natr"} && $lines{$j->{".natr"}} && $j->{".pin"} eq $lines{$writer}->{"id"})
	{
		my $jo = telex($j->{".natr"});
		$jo->{".nat"} = $writer; 
		$jo->{".pin"} = int($lines{$jo->{"_to"}}); # validate ourselves to them
		tsend($jo);
	}

	# we're asked to send something to this ip:port to open a nat
	if($j->{".nat"} && $j->{".pin"} eq $lines{$writer}->{"id"})
	{
		tsend(telex($j->{".nat"}));
	}

	# we've been told to talk to these writers
	if($j->{".see"} && $j->{"_line"})
	{
		# loop through and establish lines to them (just being dumb for now and trying everyone)
		for my $seeipp (@{$j->{".see"}})
		{
			next if($seeipp eq $ipp); # skip ourselves :)
			next if($lines{$seeipp}); # skip if we know them already
			tsend(telex($seeipp)); # send direct (should open our outgoing to them)
			# send nat request back to the writer who .see'd us in case the new one is behind a nat
			my $jo = telex($writer);
			$jo->{".natr"} = $seeipp;
			$jo->{".pin"} = int($j->{"_line"}); # need to validate our request to them to natr for us
			tsend($jo);
		}
	}

	# handle a fwd command, must be verified
	if($j->{".fwd"} && $j->{".pin"} == $lines{$writer}->{"id"})
	{
		my $e = getend($j->{".end"});
		$e->{"fwds"}->{$writer} = $j->{".fwd"};
		my $jo = telex($writer);
		$jo->{"fwds"} = $j->{".fwd"}; # just confirm whatever they sent for now
		tsend($jo);
	}

	# we do this at the end since the code needs to be refactored, and the $lines entry would only exist by now
	# track the incoming line for this writer
	$lines{$writer}->{"_line"} = int($j->{"_line"}) if($lines{$writer});
}

# create a new telex
sub telex
{
	my $to = shift;
	my $js = shift || {};
	unless($lines{$to})
	{
		# create a new line to track our relationship with this writer
		$lines{$to} = { "id" => int(rand(65535)), "first" => time(), "last" => time() };
	}
	$js->{"_to"} = $to;
	$js->{"_line"} = int($lines{$to}->{"id"});
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

# scan all known writers to keep any nat's open
sub tscan
{
	my $at = time();
	my @writers = keys %lines;
	my $purge = (scalar @writers > 1000)?1:0;
	for my $writer (@writers)
	{
		if($at - $lines{$writer}->{"last"} > 600 && $writer ne $seedipp)
		{ # ignore them if they are stale, timed out, and not our seed :)
			printf "SKIP[%s]\n",$writer;
			delete $lines{$writer} if($purge);
			next;
		}
		my $jo = telex($writer);
		$jo->{".end"} = sha1_hex($ipp);
		tsend($jo);
	}
}

# handle this telex as if we're the .end
sub doend
{
	my $t = shift;
	my $writer = shift;
	my $e = getend($t->{".end"});
	$e->{$writer} = $t; # store only one telex per writer per end
	# check for any registered fwds
	for my $wf (keys %{$e->{"fwds"}})
	{
		printf "checking fwd %s\n",$wf;
		# make sure we still have a line open to this writer, if not cancel this forward 
		if(!$lines{$wf}) # todo for later refactor, this should be cleaned up when the line is purged not here
		{
			delete $e->{"fwds"}->{$wf};
			next;
		}
		# get the actual .fwd request for this writer
		my $fwds = $e->{"fwds"}->{$wf};
		# check if any of the signals match, if so forward this telex
		my @sigs = grep($t->{$_},keys %$fwds);
		next unless(scalar @sigs > 0);
		map {$fwds->{$_}--} keys %$fwds; # decrement counters
		for my $sig (keys %$fwds) # zap any that are gone
		{
			delete $fwds->{$sig} unless($fwds->{$sig} > 0);
		}
		delete $e->{"fwds"}->{$wf} unless(scalar keys %$fwds > 0); # no more fwds, zap

		my $jo = telex($wf);
		# copy all signals
		for my $sig (grep(/^[[:alnum:]]+/, keys %$t))
		{
			$jo->{$sig} = $t->{$sig};
		}
		# copy timestamp if any
		$jo->{"_at"} = $t->{"_at"} if($t->{"_at"});
		$jo->{"fwds"} = $fwds; # tell them current status
		tsend($jo);
	}
}

sub getend
{
	my $hash = shift;
	return $ends{$hash} if($ends{$hash});
	return $ends{$hash} = {"fwds"=>{}};
}