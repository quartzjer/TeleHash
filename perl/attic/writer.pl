#!/usr/bin/perl

# a prototype telehash writer

# Jer 1/2010

use Digest::SHA1 qw(sha1_hex);
use Data::Dumper;
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

# resolve our seed to its ip:port
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
# track ip/ports to prevent excessive flooding
my %fw_ips; # track ips;
my $fw_tps = 20; # max telex per sec
my $fw_window = 5; # seconds per window
my $fw_last = int(time);
my %fw_karma; # whitelisting
my $buckets = bucket_init(); # big array of buckets 
my $lastloop = int(time);
while(1)
{
	# if we're not online, attempt to talk to the seed
	bootstrap($seedipp) if(!$connected);

	# wait for event or timeout loop
	my $newmsg = scalar $sel->can_read(10);
	if($newmsg == 0 || $lastloop+10 < int(time))
	{
		printf "LOOP\n";
		$lastloop = int(time);
		tscan();
		next if($newmsg == 0); # timeout loop
	}
	
	# must be a telex waiting for us
	$connected = 1;
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $writer = sprintf("%s:%d",inet_ntoa($addr),$cport);

	# drop if this sender is flooding us
	next if(floodwall($writer));

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
	# check to see if the _line matches or the _ring matches
	if($line->{"open"} > 0 && ($line->{"open"} != $j->{"_line"} || ($j->{"_ring"} > 0 && $line->{"open"} % $j->{"_ring"} != 0)))
	{
		print "LINE MISMATCH!\n";
		next;
	}
	# todo, should have lineto and linefrom open semantics for status checking on cmds
	if(!$line->{"open"} && ($j->{"_line"} || $j->{"_ring"}))
	{
		$line->{"open"} = $j->{"_line"} if($j->{"_line"} % $line->{"ring"} == 0); # verify their line is a product of our ring
		$line->{"open"} = int($j->{"_ring"} * $line->{"ring"}) if($j->{"_ring"}); # create a new line as a product of our ring
		bucket_see($writer,$buckets) if($line->{"open"}); # make sure they get added to a bucket too?
	}

	# first process all commands
	
	# they want recent telexes matching these signals (at least one matching signal required)
	if($j->{".hist"} && scalar grep(/^[[:alnum:]]+/, keys %$j) > 0)
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
			# XXX if we're dialing we'd want to use any of these closer to that end
			# also check to see if we want them in a bucket
			if(bucket_see($seeipp,$buckets))
			{
				tsend(tnew($seeipp)); # send direct (should open our outgoing to them)
				# send nat request back to the writer who .see'd us in case the new one is behind a nat
				my $jo = tnew($writer);
				$jo->{".natr"} = $seeipp;
				tsend($jo);				
			}
		}
	}

	# handle a fwd command, must be verified
	if($j->{".fwd"} && $j->{"_line"})
	{
		# sanitize, clean the .fwd and create a telex of just the signals and .fwd to store
		my $fwd = $j->{".fwd"};
		my %fwds = map {$_ => ($fwd->{$_}>100)?100:int($fwd->{$_})} grep(/^[[:alnum:]]+/, keys %$fwd);
		my %t = map { $_ => $j->{$_} } grep(/^[[:alnum:]]+/, keys %$j);
		# only accept it if there's at least one signal to filter on
		if(scalar keys %t > 0)
		{
			$t{".fwd"} = \%fwds;
			$forwards{$writer} = \%t; # always replace any existing
			my $jo = tnew($writer);
			$jo->{"fwds"} = \%fwds; # just confirm whatever they sent for now
			tsend($jo);
		}
	}
	
	# now process signals, if any
	next unless(grep(/^[[:alnum:]]+/,keys %$j));
	
	# a request to find other writers near this end hash
	if($j->{"end"})
	{
		# get writers from buckets near to this end
		my $cipps = bucket_near($j->{"end"},$buckets);
		my $jo = tnew($writer);
		$jo->{".see"} = $cipps;
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
		print Dumper($t);
		next unless(tmatch($t,$j));
		print "1";
		my $fwd = $t->{".fwd"};
		my @sigs = grep($j->{$_},keys %$fwd);
		next unless(scalar @sigs > 0);
		# deduct from the fwd
		print "2";
		for my $sig (grep(/^[[:alnum:]]+/, keys %$j))
		{
			$fwd->{$sig}-- if($fwd->{$sig});
			delete $fwd->{$sig} if($fwd->{$sig} <= 0);
		}
		print "3";
		# send them a copy
		my $jo = tnew($w,$j);
		$jo->{".fwd"} = $t->{".fwd"};
		tsend($jo);
		# see if the .fwd is used up
		if(scalar keys %$fwd == 0)
		{
			delete $forwards{$w};
		} 
	}
	
	# cache in history if there's any signals, max 1000
	next unless(scalar grep(/^[[:alnum:]]+/, keys %$j) > 0);
	$j->{"at"} = time() unless($j->{"at"}); # make sure an at signal is set
	unshift(@history,$j);
	@history = splice(@history,0,1000);
}

# for creating and tracking lines to writers
sub getline
{
	my $writer = shift;
	if(!$lines{$writer})
	{
		printf "LINE[%s]\n",$writer;		
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

# actually send a telex to its writer
sub tsend
{
	my $j = shift;
	my($ip,$port) = split(":",$j->{"_to"});
	my $wip = gethostbyname($ip);
	return unless($wip); # bad ip?
	my $waddr = sockaddr_in($port,$wip);
	return unless($waddr); # bad port?
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
	my @match = grep {$t2->{$_} eq $t1->{$_}} @sigs;
	return (scalar @sigs == scalar @match); 
}

# scan all known writers to keep any nat's open
sub tscan
{
	my $at = time();
	my @writers = keys %lines;
	for my $writer (@writers)
	{
		next if($writer eq $ipp); # ??
		delete $lines{$writer}->{"open"} if($at - $lines{$writer}->{"last"} > 300); # remove open line status if older than 5min
		if($at - $lines{$writer}->{"last"} > 600)
		{ # remove them if they are stale, timed out
			printf "PURGE[%s]\n",$writer;
			$lines{$writer} = undef;
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
	# make sure seed is in a bucket
	bucket_see($seed,$buckets);
}

# create the array of buckets
sub bucket_init
{
	my @ba;
	for my $pos (0..159)
	{
		$ba[$pos] = {};
	}
	return \@ba;
}

# find active writers
sub bucket_near
{
	my $end = shift;
	my $buckets = shift;
	my $bto = bix_new($end); # convert to format for the big xor for faster sorting
	my $bme = bix_new($ipphash);
	my $start = bix_sbit(bix_or($bto,$bme));
	$start = 0 if($start < 0 || $start > 159); # err, this needs to be handled better or something
	my @ret;
	# first check all buckets closer
	printf "NEAR[%d %s] ",$start,$end;
	my $pos = $start+1;
	while(--$pos)
	{
#printf "%d/%d %s",$pos,scalar @ret,Dumper($buckets->[$pos]);
		push @ret,grep($lines{$_}->{"open"},keys %{$buckets->[$pos]}); # only push active writers
		last if(scalar @ret >= 5);
	}
	# the check all buckets further
	for my $pos (($start+1) .. 159)
	{
#printf "%d/%d ",$pos,scalar @ret;
		push @ret,grep($lines{$_}->{"open"},keys %{$buckets->[$pos]}); # only push active writers
		last if(scalar @ret >= 5);
	}
	my %hashes = map {sha1_hex($_)=>$_} @ret;
	$hashes{$ipphash} = $ipp; # include ourselves always
	my @bixes = map {bix_new($_)} keys %hashes; # pre-bix the array for faster sorting
	my @ckeys = sort {bix_cmp(bix_or($bto,$a),bix_or($bto,$b))} @bixes; # sort by closest to the end
	printf("from %d writers, closest is %d\n",scalar @ckeys, bix_sbit(bix_or($bto,$ckeys[0])));
	@ret = map {$hashes{bix_str($_)}} splice @ckeys, 0, 5; # convert back to ip:ports, top 5
	return \@ret;
}

# see if we want to try this writer or not, and maybe prune the bucket
sub bucket_see
{
	my $writer = shift;
	my $buckets = shift;
	my $pos = bix_sbit(bix_or(bix_new($writer),bix_new($ipphash)));
	printf "BUCKET[%d %s]\n",$pos,$writer;
	return undef if($pos < 0 || $pos > 159); # err!?
	$buckets->[$pos]->{$writer}++;
	return 1; # for now we're always taking everyone, in future need to be more selective when the bucket is "full"!
}

sub floodwall
{
	my $writer = shift;
	# first check if it's a karma setting
	my $karma = shift;
	if($karma)
	{
		$fw_karma{$writer}+=$karma;
		return undef;
	}

	# if karma saved, decide on own
	if($fw_karma{$writer})
	{
		$fw_karma{$writer}--;
		return undef if($fw_karma{$writer} > 0);
		delete $fw_karma{$writer};
	}

	# now, if we're in a new window, reset
	my $at = int(time);
	if($fw_last+5 < $at)
	{
		%fw_ips = ();
		$fw_last = $at;
	}

	# count packets per ip
	my($ip,$port) = split(":",$writer);
	$fw_ips{$ip}++;

	# if the IP is OK
	return undef if($fw_ips{$ip} < $fw_window*$fw_tps);
	
	# too many, FAIL
	printf "FLOODWALL[%s]\n",$writer;
	return 1;
}
