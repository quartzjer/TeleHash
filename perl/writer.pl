#!/usr/bin/perl

# a prototype telehash writer

# Jer 1/2010
# big reorg 4/2010

use Digest::SHA1 qw(sha1_hex);
use Data::Dumper;
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;
require "./bixor.pl"; # pardon the lame style of code reuse

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
my %forwards; # writers with active .fwd going
my $buff;
$|++;
my $ipp, $ipphash;
my $connected=undef;
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
		linecheck();
		next if($newmsg == 0); # timeout loop
	}
	
	# must be a telex waiting for us
	$connected = 1;
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");

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
	my $line = getline($writer,$j->{"_ring"});
	my $lstat = checkline($line,$j);
	if(!$lstat)
	{
		printf "LINE FAIL[%s]\n",$json->to_json($line);
		next;
	}else{
		printf "LINE STATUS %s\n",$j->{"_line"}?"OPEN":"RINGING";
	}

	# the only thing we can do before we have an "open" line is process any signals
	if(grep(/^[[:alnum:]]+/,keys %$j))
	{
		# any first-hop end signal is responded to
		if($j->{"end"} && int($j->{"_hop"}) == 0)
		{
			# get writers from buckets near to this end
			my $cipps = bucket_near($j->{"end"},$buckets);
			my $jo = tnew($writer);
			$jo->{".see"} = $cipps;
			tsend($jo);			
		}

		# todo: create a sig/val hash, check for unique and cache it
		# if not last-hop, check for any active forwards (todo: optimize the matching, this is just brute force)
		if(int($j->{"_hop"}) < 4)
		{
			for my $w (keys %forwards)
			{
				my $t = $forwards{$w};
				print Dumper($t);
				my @sigs = grep(/^[[:alnum:]]+/, keys %$t1);
				my @match = grep {$t2->{$_} eq $t1->{$_}} @sigs;
				return (scalar @sigs == scalar @match); 
				print "1";
				my $fwd = $t->{".fwd"};
				my @sigs = grep($j->{$_},keys %$fwd);
				next unless(scalar @sigs > 0);
				print "3";
				# send them a copy
				my $jo = tnew($w,$j);
				$jo->{"_hop"} = int($t->{"_hop"})+1;
				tsend($jo);
			}
		}
	}
	
	# to process any commands we need an open line (since we always send a ring and expect a line back)
	next unless(!$j->{"_line"});
	
	# a request to send a .nat to a writer that we should know (and only from writers we have a line to)
	if($j->{".natr"} && $lines{$j->{".natr"}})
	{
		my $jo = tnew($j->{".natr"});
		$jo->{".nat"} = $writer; 
		tsend($jo);
	}

	# we're asked to send something to this ip:port to open a nat
	if($j->{".nat"})
	{
		tsend(tnew($j->{".nat"}));
	}

	# we've been told to talk to these writers
	if($j->{".see"})
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
	if($j->{".fwd"})
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
}

# for creating and tracking lines to writers
sub getline
{
	my $writer = shift;
	if(!$lines{$writer})
	{
		printf "LINE[%s]\n",$writer;
		$lines{$writer} = { ipp=>$writer, ringout=>int(rand(32768)+1), seenat=>0, sentat=>0, limboout=>0, limboin=>0, lineat=>0 };
	}
}

# validate a telex with incoming ring/line vars
sub checkline
{
	my $line = shift;
	my $t = shift;
	
	# first, if it's been more than 10 seconds after a line opened, be super strict, no more ringing allowed, _line absolutely required
	if($line->{lineat} > 0 && time() - $line->{lineat} > 10)
	{
		return undef unless($t->{_line} == $line->{line});
	}

	# second, process incoming _line
	if($t->{_line})
	{
		return undef if($line->{line} && $t->{_line} != $line->{line}); # must match if exist
		return undef if($t->{_line} % $line->{ringout} != 0); # must be a product of our sent ring!!
		# we can set up the line now if needed
		if($line->{lineat} == 0)
		{
			$line->{ringin} = $t->{_line} / $line->{ringout}; # will be valid if the % = 0 above
			$line->{line} = $t->{_line};
			$line->{lineat} = time();
			bucket_see($line->{ipp},$buckets); # make sure they get added to a bucket too
		}
	}

	# last, process any incoming _ring's (remember, could be out of order, after a _line)
	if($t->{_ring})
	{
		return undef if($line->{ringin} && $t->{_ring} != $line->{ringin}); # already had a ring and this one doesn't match, should be rare
		return undef unless($t->{_ring} > 0 && $t->{_ring} <= 32768); # make sure within valid range
		# we can set up the line now if needed
		if($line->{lineat} == 0)
		{
			$line->{ringin} = $t->{_ring};
			$line->{line} = $line->{ringin} * $line->{ringout};
			$line->{lineat} = time();
			bucket_see($line->{ipp},$buckets); # make sure they get added to a bucket too
		}
	}

	return 1;
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
	return unless($wip); # bad ip?
	my $waddr = sockaddr_in($port,$wip);
	return unless($waddr); # bad port?
	my $line = getline($j->{"_to"});
	# update our limbo tracking and send current state
	$j->{"_limbo"} = $line->{"limbo"};
	my $js = $json->to_json($j);
	$line->{"limbo"} += length($js);
	printf "SEND[%s]\t%s\n",$j->{"_to"},$js;
	if(!defined(send(SOCKET, $js, 0, $waddr)))
	{
		$ipp=$connected=undef;
		printf "OFFLINE\n";	
	}
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
