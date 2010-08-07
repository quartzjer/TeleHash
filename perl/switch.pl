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
my %taps;
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
	
	# if there's an open line, we can process commands, yay
	if($j->{"_line"})
	{
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
					# send direct (should open our outgoing to them)
					my $jo = tnew($seeipp);
					$jo->{"+end"} = $ipphash;
					tsend($jo);
					# send pop signal back to the switch who .see'd us in case the new one is behind a nat
					my $jo = tnew($switch);
					$jo->{"+end"} = sha1_hex($seeipp);
					$jo->{"+pop"} = "th:$ipp";
					tsend($jo);
				}
			}
		}

		# handle a tap command, add/replace rules
		if($j->{".tap"} && ref $j->{".tap"} eq "ARRAY")
		{
			$line->{"rules"} = $j->{".tap"};
			tapwipe($switch);
			# walk every rule, any possible sigs are added to a sort of index to help in filtering incoming
			for my $rule (@{$line->{"rules"}})
			{
				for my $sig (keys %{$rule->{is}})
				{
					tapadd($switch,$sig);
				}
				for my $sig (@{$rule->{has}})
				{
					tapadd($switch,$sig);
				}
			}
		}
	}

	# no open line, or after we processed commands, we can process any signals
	if(grep(/^\+.+/,keys %$j))
	{
		# any first-hop end signal is responded to
		if($j->{"+end"} && int($j->{"_hop"}) == 0)
		{
			# get switches from buckets near to this end
			my $cipps = bucket_near($j->{"+end"},$buckets);
			my $jo = tnew($switch);
			# TODO: this is where dampening should happen to not advertise switches that might be too busy
			$jo->{".see"} = $cipps;
			tsend($jo);
		}
		
		# this is our .tap, requests to +pop for NATs
		if($j->{"_hop"} == 1 && $j->{"+end"} eq $ipphash && $j->{"+pop"} =~ /th\:([\d\.]+)\:(\d+)/)
		{
			my $ip = $1;
			my $port = $2;
			tsend(tnew("$ip:$port"));
		}

		# if not last-hop, check for any active taps (todo: optimize the matching, this is just brute force)
		if(int($j->{"_hop"}) < 4)
		{
			my %switches;
			for my $sig (grep($taps{$_},keys %$j))
			{
				for my $sw (keys %{$taps{$sig}})
				{
					$switches{$sw}++;
				}
			}
			printf "\tTAP POSSIBLE SWITCHES %d\n",scalar keys %switches;
			for my $sw (keys %switches)
			{
				my $pass=0;
				for my $rule (@{$lines{$sw}->{"rules"}})
				{
					printf "\tTAP CHECK IS %s\t%s\n",$sw,$json->to_json($rule);
					# all the "is" are in this telex and match exactly
					next unless(scalar grep {$j->{$_} eq $rule->{"is"}->{$_}} keys %{$rule->{"is"}} == scalar keys %{$rule->{"is"}});
					# pass fail if any has doesn't exist
					$pass++;
					for my $sig (@{$rule->{"has"}})
					{
						$pass = 0 unless($j->{$sig});
					}
					last;
				}
				# forward this switch a copy
				if($pass)
				{
					my $jo = tnew($sw);
					for my $sig (grep(/^\+.+/, keys %$j))
					{
						$jo->{$sig} = $j->{$sig};
					}
					$jo->{"_hop"} = int($t->{"_hop"})+1;
					tsend($jo);
				}else{
					printf "\tCHECK MISS\n";
				}
			}
		}
	}
}

# add a tap indicator for this switch/signal
sub tapadd
{
	my $switch = shift;
	my $sig = shift;
	$taps{$sig} = {} unless($taps{$sig}); # new blank
	$taps{$sig}->{$switch}++; # just flag it exists
}

# remove all taps for a switch
sub tapwipe
{
	my $switch = shift;
	for my $sig (keys %taps)
	{
		delete $taps{$sig}->{$switch};
	}
}

# for creating and tracking lines to switches
sub getline
{
	my $switch = shift;
	if(!$lines{$switch} || $lines{$switch}->{ipp} ne $switch)
	{
		printf "\tNEWLINE[%s]\n",$switch;
		my($ip,$port) = split(":",$switch);
		my $wip = gethostbyname($ip);
		return undef unless($wip); # bad ip?
		my $addr = sockaddr_in($port,$wip);
		return undef unless($addr); # bad port?
		$lines{$switch} = { ipp=>$switch, addr=>$addr, ringout=>int(rand(32768))+1, init=>time(), seenat=>0, sentat=>0, lineat=>0, br=>0, brout=>0, brin=>0, bsent=>0 };
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
		return undef unless($line->{ringout} > 0);
		$t->{_line} = int($t->{_line}); # be nice in what we accept, strict in what we send
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
printf "\tBR %s [%d += %d] DIFF %d\n",$line->{ipp},$line->{br},$br,$line->{bsent} - $t->{_br};
	$line->{br} += $br;
	$line->{brin} = $t->{_br};
	return undef if($line->{br} - $line->{brout} > 12000); # they can't send us that much more than what we've told them to, bad!

	# XXX if this is the first seenat, if we were dialing we might need to re-send our telex as this could be a nat open pingback
	$line->{"seenat"} = time();

	return 1;
}

# create a new telex
sub tnew
{
	my $to = shift;
	my $js = {};
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
		printf "\tMAX SEND DROP\n";
		return;
	}
	# if a line is open use that, else send a ring
	if($line->{"line"})
	{
		$j->{"_line"} = int($line->{"line"});		
	}else{
		$j->{"_ring"} = int($line->{"ringout"});
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
		if(($line->{"seenat"} == 0 && $at - $line->{"init"} > 70) || ($line->{"seenat"} != 0 && $at - $line->{"seenat"} > 70))
		{ # remove them if they never responded or haven't in a while
			printf "\tPURGE[%s]\n",$switch;
			$lines{$switch} = {};
			next;
		}
		# end ourselves to see if they know anyone closer as a ping
		my $jo = tnew($switch);
		$jo->{"+end"} = $ipphash;
		# also .see ourselves, default for now is to participate in the DHT
		$jo->{".see"} = [$ipp];
		# also .tap our hash for +pop requests for NATs
		$jo->{".tap"} = [{"is"=>{"+end"=>$ipphash},"has"=>["+pop"]}];
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
	return $ends{$hash} = {"taps"=>{}};
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