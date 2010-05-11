#!/usr/bin/perl

# a prototype telehash switch

# Jer 1/2010
# big reorg 4/2010

use Digest::SHA1 qw(sha1_hex);
use Data::Dumper;
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

my %lines; # static line assignments per switch
my %fwds;
my $buff;
$|++;
my $ipp, $ipphash;
my $connected=undef;
my $buckets = bucket_init(); # big array of buckets 
my $lastloop = int(time);
while(1)
{
	# if we're not online, attempt to talk to the seed
	bootstrap($seedipp) if(!$connected && $ipp ne $seedipp);

	# wait for event or timeout loop
	my $newmsg = scalar $sel->can_read(10);
	if($newmsg == 0 || $lastloop+10 < int(time))
	{
		printf "LOOP\n";
		$lastloop = int(time);
		scanlines();
		next if($newmsg == 0); # timeout loop
	}
	
	# must be a telex waiting for us
	$connected = 1;
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $switch = sprintf("%s:%d",inet_ntoa($addr),$cport);

	printf "RECV[%s]\t%s\n",$switch,$buff;

	# json parse check
	my $j = $json->from_json($buff) || next;

	# FIRST, if we're bootstrapping, discover our own ip:port
	if(!$ipp && $j->{"_to"})
	{
		printf "\tSELF[%s]\n",$j->{"_to"};
		$ipp = $j->{"_to"};
		$ipphash = sha1_hex($ipp);
		# WE are the seed, haha, remove our own line and skip
		if($ipp eq $switch)
		{
			printf "\tWe're the seed!\n";
			delete $lines{$ipp};
			next;
		}
	}

	# if this is a switch we know, check a few things
	my $line = getline($switch,$j->{"_ring"});
	my $lstat = checkline($line,$j,length($buff));
	if(!$lstat)
	{
		printf "\tLINE FAIL[%s]\n",$json->to_json($line);
		next;
	}else{
		printf "\tLINE STATUS %s\n",$j->{"_line"}?"OPEN":"RINGING";
	}
	
	# the only thing we can do before we have an "open" line is process any signals
	if(grep(/^\+.+/,keys %$j))
	{
		# any first-hop end signal is responded to
		if($j->{"+end"} && int($j->{"_hop"}) == 0)
		{
			# get switches from buckets near to this end
			my $cipps = bucket_near($j->{"+end"},$buckets);
			my $jo = tnew($switch);
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
	
	# a request to send a .nat to a switch that we should know (and only from switches we have a line to)
	if($j->{".natr"} && $lines{$j->{".natr"}})
	{
		my $jo = tnew($j->{".natr"});
		$jo->{".nat"} = $switch; 
		tsend($jo);
	}

	# we're asked to send something to this ip:port to open a nat
	if($j->{".nat"})
	{
		tsend(tnew($j->{".nat"}));
	}

	# we've been told to talk to these switches
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
				# send nat request back to the switch who .see'd us in case the new one is behind a nat
				my $jo = tnew($switch);
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
			$forwards{$switch} = \%t; # always replace any existing
			my $jo = tnew($switch);
			$jo->{"fwds"} = \%fwds; # just confirm whatever they sent for now
			tsend($jo);
		}
	}
}

# add a fwd rule for a switch
sub fwdadd
{
	my $switch = shift;
	my $sig = shift;
	my $val = shift; # optional, for "has"
	$fwds{$sig} = {} unless($fwds{$sig}); # new blank
	$fwds{$sig}->{$switch} = ($val)?$val:[];
}

# remove all fwds for a switch
sub fwdwipe
{
	my $switch = shift;
	for my $sig (keys %fwds)
	{
		
	}
}

# for creating and tracking lines to switches
sub getline
{
	my $switch = shift;
	if(!$lines{$switch})
	{
		printf "\tNEWLINE[%s]\n",$switch;
		my($ip,$port) = split(":",$switch);
		my $wip = gethostbyname($ip);
		return undef unless($wip); # bad ip?
		my $addr = sockaddr_in($port,$wip);
		return undef unless($addr); # bad port?
		$lines{$switch} = { ipp=>$switch, addr=>$addr, ringout=>int(rand(32768)+1), init=>time(), seenat=>0, sentat=>0, lineat=>0, br=>0, brout=>0, brin=>0, bsent=>0 };
	}
	return $lines{$switch};
}

# validate a telex with incoming ring/line headers
sub checkline
{
	my $line = shift;
	my $t = shift;
	my $br = shift;
	
	return undef unless($line);

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

	# we're valid at this point, line or otherwise, track bytes
printf "\tBR %s [%d += %d]\n",$line->{ipp},$line->{br},$br;
	$line->{br} += $br;
	return undef if($line->{br} - $line->{brout} > 12000); # they can't send us that much more than what we've told them to, bad!

	$line->{"seenat"} = time();

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
	$js->{"_to"} = $to;
	return $js;
}

# actually send a telex to it's switch, add/track all line headers
sub tsend
{
	my $j = shift;
	my $line = getline($j->{"_to"}) || return undef;
	# check br and drop if too much
	if($line->{"bsent"} - $line->{"brin"} > 10000)
	{
		printf "\tMAX SEND DROP";
		return;
	}
	# if a line is open use that, else send a ring
	if($line->{"line"})
	{
		$j->{"_line"} = $line->{"line"};		
	}else{
		$j->{"_ring"} = $line->{"ringout"};
	}
	# update our bytes tracking and send current state
	$j->{"_br"} = $line->{"brout"} = $line->{"br"};
	my $js = $json->to_json($j);
	$line->{"bsent"} += length($js);
	$line->{"sentat"} = time();
	printf "\tSEND[%s]\t%s\n",$j->{"_to"},$js;
	if(!defined(send(SOCKET, $js, 0, $line->{"addr"})))
	{
		$ipp=$connected=undef;
		printf "\tOFFLINE\n";	
	}
}

# scan all known switches to keep any nat's open
sub scanlines
{
	my $at = time();
	my @switches = keys %lines;
	for my $switch (@switches)
	{
		next if($switch eq $ipp); # ??
		my $line = $lines{$switch};
		if(($line->{"seenat"} == 0 && $at - $line->{"init"} > 70) || $at - $line->{"seenat"} > 70)
		{ # remove them if they never responded or haven't in a while
			printf "\tPURGE[%s]\n",$switch;
			delete $lines{$switch};
			next;
		}
		# end ourselves to see if they know anyone closer as a ping
		my $jo = tnew($switch);
		$jo->{"+end"} = sha1_hex($ipp);
		tsend($jo);
	}
	# if no lines and we're not the seed
	if(scalar keys %lines == 0 && $ipp ne $seedipp)
	{
		$ipp=$connected=undef;
		printf "\tOFFLINE\n";	
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
	printf "SEEDING[%s]\n",$seed;
	my $jo = tnew($seed);
	# make sure the hash is really far away so they .see us back a bunch
	$jo->{"+end"} = bix_str(bix_far(bix_new(sha1_hex($seed))));
	tsend($jo);
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

# find active switches
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
	printf "\tNEAR[%d %s] ",$start,$end;
	my $pos = $start+1;
	while(--$pos)
	{
#printf "%d/%d %s",$pos,scalar @ret,Dumper($buckets->[$pos]);
		push @ret,grep($lines{$_}->{"lineat"},keys %{$buckets->[$pos]}); # only push active switches
		last if(scalar @ret >= 5);
	}
	# the check all buckets further
	for my $pos (($start+1) .. 159)
	{
#printf "%d/%d ",$pos,scalar @ret;
		push @ret,grep($lines{$_}->{"lineat"},keys %{$buckets->[$pos]}); # only push active switches
		last if(scalar @ret >= 5);
	}
	my %hashes = map {sha1_hex($_)=>$_} @ret;
	$hashes{$ipphash} = $ipp; # include ourselves always
	my @bixes = map {bix_new($_)} keys %hashes; # pre-bix the array for faster sorting
	my @ckeys = sort {bix_cmp(bix_or($bto,$a),bix_or($bto,$b))} @bixes; # sort by closest to the end
	printf("\tfrom %d switches, closest is %d\n",scalar @ckeys, bix_sbit(bix_or($bto,$ckeys[0])));
	@ret = map {$hashes{bix_str($_)}} splice @ckeys, 0, 5; # convert back to ip:ports, top 5
	return \@ret;
}

# see if we want to try this switch or not, and maybe prune the bucket
sub bucket_see
{
	my $switch = shift;
	my $buckets = shift;
	my $pos = bix_sbit(bix_or(bix_new($switch),bix_new($ipphash)));
	printf "\tBUCKET[%d %s]\n",$pos,$switch;
	return undef if($pos < 0 || $pos > 159); # err!?
	$buckets->[$pos]->{$switch}++;
	return 1; # for now we're always taking everyone, in future need to be more selective when the bucket is "full"!
}


# including these here for convenience
# takes hex string
sub bix_new
{
	my @bix;
	for my $b (split undef,shift)
	{
		push @bix,hex $b;
	}
	return \@bix;
}
sub bix_str
{
	my $br = shift;
	my $str;
	for (@$br)
	{
		$str .= sprintf "%x",$_;
	}
	return $str;
}
sub bix_or
{
	my $a = shift;
	my $b = shift;
	my @c;
	for my $i (0..39)
	{
		$c[$i] = $a->[$i] ^ $b->[$i];
	}
	return \@c;
}

sub bix_cmp
{
	my $a = shift;
	my $b = shift;
	for my $i (0..39)
	{
		next if($a->[$i] == $b->[$i]);
		return $a->[$i] <=> $b->[$i];
	}
	return 0;
}

# invert the bits, or make a hash as far away as possible
sub bix_far
{
	my $a = shift;
	my @c;
	for my $i (0..39)
	{
		$c[$i] = $a->[$i] ^ hex 'f';
	}
	return \@c;
}

sub bix_sbit
{
	my $b = shift;
	my @sbtab = (-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3); # ln(hex)/log(2)
	my $ret = 156;
	for my $i (0..39)
	{
		return $ret + $sbtab[$b->[$i]] if($b->[$i]);
		$ret -= 4;
	}
	return -1;
}