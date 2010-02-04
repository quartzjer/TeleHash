#!/usr/bin/perl

# a prototype telehash writer

# Jer 1/2010

use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;
require "./bixor.pl"; # temp testing hack

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

my %lines; # static line assignments per writer
my %ends; # any end hashes that we've handled
my @history; # last however many telexes (containing signals)
my %forwards; # writers with active .fwd going
my $buff;
$|++;
my $ipp, $ipphash;
my $connected=undef;
while(1)
{
	# if we're not online, attempt to talk to the seed
	bootstrap($seedipp) if(!$connected);

	# wait for event or timeout loop
	if(scalar $sel->can_read(10) == 0)
	{
		printf "LOOP\n";
		tscan();
		next;
	}
	
	# must be a telex waiting for us
	$connected = 1;
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");
	# TODO need some source rate detection in case there's a loop

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $writer = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "RECV[%s]\t%s\n",$writer,$buff;

	# json parse check
	my $j = $json->from_json($buff) || next;

	# FIRST, if we're bootstrapping, discover our own ip:port
	if(!$ipp && $j->{"_to"})
	{
		printf "SELF[%s]\n",$j->{"_to"};
		$ipp = $j->{"_to"};
		$ipphash = sha1_hex($ipp);
		# WE are the seed, haha, remove our own line and skip
		if($ipp eq $writer)
		{
			delete $lines{$ipp};
			next;
		}
	}

	# if this is a writer we know, check a few things
	my $line = getline($writer);
	# keep track of when we've last got stuff from them
	$line->{"last"} = time();
	# check to see if the line matches, and skip if not
	if($line->{"open"} && $line->{"open"} != $j->{"_line"})
	{
		print "LINE MISMATCH!\n";
		next;
	}
	# todo, should have lineto and linefrom open semantics for status checking on cmds
	if(!$line->{"open"} && ($j->{"_line"} || $j->{"_ring"}))
	{
		$line->{"open"} = $j->{"_line"} if($j->{"_line"} % $line->{"ring"} == 0); # verify their line is a product of our ring
		$line->{"open"} = int($j->{"_ring"} * $line->{"ring"}) if($j->{"_ring"}); # create a new line as a product of our ring
	}

	# first process all commands
	
	# they want recent telexes matching these signals
	if($j->{".hist"})
	{
		my $hist = $j->{".hist"};
		# sanitize hist request
		my %hists = map {$_ => $hist->{$_}} grep(/^[[:alnum:]]+/, keys %$hist);
		# loop through all history new to old to find any matches
		for my $t (@history)
		{
			# first make sure any of the requested signals exist
			my @sigs = grep($t->{$_},keys %hists);
			next unless(scalar @sigs > 0);
			next unless(tmatch($j,$t));
			# deduct from the hist request
			for my $sig (grep(/^[[:alnum:]]+/, keys %$t))
			{
				$hists{$sig}-- if($hists{$sig});
				delete $hists{$sig} if($hists{$sig} <= 0);
			}
			# send them a copy
			tsend(tnew($writer,$t));
			# see if their request is used up
			last unless(scalar keys %hists > 0); 
		}
	}

	# a request to send a .nat to a writer that we should know (and only from writers we have a line to)
	if($j->{".natr"} && $lines{$j->{".natr"}} && $j->{"_line"})
	{
		my $jo = tnew($j->{".natr"});
		$jo->{".nat"} = $writer; 
		tsend($jo);
	}

	# we're asked to send something to this ip:port to open a nat
	if($j->{".nat"} && $j->{"_line"})
	{
		tsend(tnew($j->{".nat"}));
	}

	# we've been told to talk to these writers
	if($j->{".see"} && $line->{"open"})
	{
		# loop through and establish lines to them (just being dumb for now and trying everyone)
		for my $seeipp (@{$j->{".see"}})
		{
			next if($seeipp eq $ipp); # skip ourselves :)
			next if($lines{$seeipp}); # skip if we know them already
			tsend(tnew($seeipp)); # send direct (should open our outgoing to them)
			# send nat request back to the writer who .see'd us in case the new one is behind a nat
			my $jo = tnew($writer);
			$jo->{".natr"} = $seeipp;
			tsend($jo);
		}
	}

	# handle a fwd command, must be verified
	if($j->{".fwd"} && $j->{"_line"})
	{
		# sanitize, clean the .fwd and create a telex of just the signals and .fwd to store
		my $fwd = $j->{".fwd"};
		my %fwds = map {$_ => ($fwd->{$_}>100)?100:int($fwd->{$_})} grep(/^[[:alnum:]]+/, keys %$fwd);
		my %t = map { $_ => $j->{$_} } grep(/^[[:alnum:]]+/, keys %$j);
		$t{".fwd"} = \%fwds;
		$forwards{$writer} = \%t; # always replace any existing
		my $jo = tnew($writer);
		$jo->{"fwds"} = \%fwds; # just confirm whatever they sent for now
		tsend($jo);
	}
	
	# now process signals, if any
	next unless(grep(/^[[:alnum:]]+/,keys %$j));
	
	# a request to find other writers near this end hash
	if($j->{"end"})
	{
		my $bto = bix_new($j->{"end"}); # convert to format for the big xor for faster sorting
		my %hashes = map {sha1_hex($_)=>$_} grep($lines{$_}->{"open"}, keys %lines); # must have open line to announce them
		my @bixes = map {bix_new($_)} keys %hashes; # pre-bix the array for faster sorting
		my @ckeys = sort {bix_cmp(bix_or($bto,$a),bix_or($bto,$b))} @bixes; # sort by closest to the end
		printf("from %d writers, closest is %d\n",scalar @ckeys, bix_sbit(bix_or($bto,$ckeys[0])));
		# send them back a .see of whatever is the closest we know
		my @cipps = map {$hashes{bix_str($_)}} splice @ckeys, 0, 5; # convert back to ip:ports, top 5
		my $jo = tnew($writer);
		$jo->{".see"} = \@cipps;
		tsend($jo);			
		# if we're the closest, we should try to cache this in the history longer
		if(bix_str($ckeys[0]) eq $ipphash)
		{
			printf("closest!\n");
		}
	}
	
	# check for any active forwards (todo: optimize the matching, this is just brute force)
	for my $w (keys %forwards)
	{
		my $t = $forwards{$w};
		next unless(tmatch($t,$j));
		my $fwd = $t->{".fwd"};
		my @sigs = grep($t->{$_},keys %$fwd);
		next unless(scalar @sigs > 0);
		# deduct from the fwd
		for my $sig (grep(/^[[:alnum:]]+/, keys %$t))
		{
			$fwd->{$sig}-- if($fwd->{$sig});
			delete $fwd->{$sig} if($fwd->{$sig} <= 0);
		}
		# send them a copy
		my $jo = tnew($writer,$t);
		$jo->{".fwd"} = $t->{".fwd"};
		tsend($jo);
		# see if the .fwd is used up
		if(scalar keys %$fwd == 0)
		{
			delete $forwards{$w};
		} 
	}
	
	# cache in history, max 1000
	$j->{"_at"} = time() unless($j->{"_at"}); # make sure _at is set
	unshift(@history,$j);
	@history = splice(@history,0,1000);
}

# for creating and tracking lines to writers
sub getline
{
	my $writer = shift;
	if(!$lines{$writer})
	{
		$lines{$writer} = { "ring" => int(rand(32768)), "first" => time(), "last" => time() };
	}
	return $lines{$writer};
}

# create a new telex
sub tnew
{
	my $to = shift;
	my $clone = shift;
	my $js = {};
	# if there's a telex sent, clone all signals from it
	for my $sig (grep(/^[[:alnum:]]+/, keys %$clone))
	{
		$js->{$sig} = $clone->{$sig};
	}
	my $line = getline($to);
	# if a line is open use that, else send a ring
	if($line->{"open"})
	{
		$js->{"_line"} = int($line->{"open"});		
	}else{
		$js->{"_ring"} = int($line->{"ring"});
	}
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
	if(!defined(send(SOCKET, $js, 0, $waddr)))
	{
		$ipp=$connected=undef;
		printf "OFFLINE\n";	
	}
}

# see if the second telex is a match or superset of the first's signals
sub tmatch
{
	my $t1 = shift;
	my $t2 = shift;
	my @sigs = grep(/^[[:alnum:]]+/, keys %$t1);
	my @match = scalar map {$t2->{$_} eq $t1->{$_}} @sigs;
	return (scalar @sigs == scalar @match); 
}

# scan all known writers to keep any nat's open
sub tscan
{
	my $at = time();
	my @writers = keys %lines;
	for my $writer (@writers)
	{
		delete $lines{$writer}->{"open"} if($at - $lines{$writer}->{"last"} > 300); # remove open line status if older than 5min
		if($at - $lines{$writer}->{"last"} > 600)
		{ # remove them if they are stale, timed out
			printf "PURGE[%s]\n",$writer;
			delete $lines{$writer};
			next;
		}
		my $jo = tnew($writer);
		$jo->{"end"} = sha1_hex($ipp);
		tsend($jo);
	}
	if(scalar keys %lines == 0)
	{
		$ipp=$connected=undef;
		printf "OFFLINE\n";	
	}
}

sub getend
{
	my $hash = shift;
	return $ends{$hash} if($ends{$hash});
	return $ends{$hash} = {"fwds"=>{}};
}

# send a hello to the seed
sub bootstrap
{
	my $seed = shift;
	my $jo = tnew($seed);
	# make sure the hash is really far away so they .see us back a bunch
	$jo->{"end"} = bix_str(bix_far(bix_new(sha1_hex($seed))));
	tsend($jo);
}

