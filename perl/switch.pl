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
use Getopt::Std;
getopt("pset"); # port seed end tap
printf STDERR "options port '%s' seed '%s' end '%s' tap '%s'\n",$opt_p,$opt_s,$opt_e,$opt_t;
my $port = $opt_p||0;
my $ip = "0.0.0.0"; 
my $seed = $opt_s||"telehash.org:42424";
my $tap_end = $opt_e;
my $tap_js = $opt_t?$json->from_json($opt_t):undef;
my 

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

my %master; # all known switches, key is sha1 hex
my %taps;
my $buff;
$|++;
my $selfipp, $selfhash;
my $connected=undef;
my $buckets;
my $lastloop = int(time);
while(1)
{
	# if we're not online, attempt to talk to the seed
	bootstrap($seedipp) if(!$connected && $selfipp ne $seedipp);

	# wait for event or timeout loop
	my $newmsg = scalar $sel->can_read(1);
	if($newmsg == 0 || $lastloop+1 < int(time))
	{
		printf STDERR "LOOP %d\t%d\t%d\t%d\n",$newmsg,$lastloop,$connected,$tap_end;
		$lastloop = int(time);
		if($connected)
		{
			scanlines();
			taptap() if($tap_end);
		}
		next if($newmsg == 0); # timeout loop
	}
	
	# must be a telex waiting for us
	$connected = 1;
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $remoteipp = sprintf("%s:%d",inet_ntoa($addr),$cport);

	printf STDERR "\nRECV[%s]\t%s\n",$remoteipp,$buff;

	# json parse check
	my $j = $json->from_json($buff) || next;

	# FIRST, if we're bootstrapping, discover our own ip:port
	if(!$selfipp && $j->{"_to"})
	{
		printf STDERR "\tSELF[%s]\n",$j->{"_to"};
		$selfipp = $j->{"_to"};
		$selfhash = sha1_hex($selfipp);
		my $line = getline($selfipp);
		$line->{visible}=1; # flag ourselves as default visible
		$line->{rules} = $tap_js; # if we're tap'ing anything
		if($selfipp eq $remoteipp)
		{
			printf STDERR "\tWe're the seed!\n";
			next;
		}
	}

	# if this is a switch we know, check a few things
	my $line = getline($remoteipp,$j->{"_ring"});
	my $lstat = checkline($line,$j,length($buff));
	if(!$lstat)
	{
		printf STDERR "\tLINE FAIL[%s]\n",$json->to_json($line);
		next;
	}else{
		printf STDERR "\tLINE STATUS %s\n",$j->{"_line"}?"OPEN":"RINGING";
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
				next if($seeipp eq $selfipp); # skip ourselves :)
				# they're making themselves visible now, awesome
				if($seeipp eq $remoteipp && !$line->{visible})
				{
					printf STDERR "\t\tVISIBLE %s\n",$remoteipp;
					$line->{visible}=1;
					map {$line->{neighbors}->{$_}=1} near_to($line->{end},$selfipp); # load values into this switch's neighbors list, start from ourselves
					near_to($line->{end},$remoteipp); # injects this switch as hints into it's neighbors, fully seeded now
				}
				next if($master{sha1_hex($seeipp)}); # skip if we know them already
				# XXX todo: if we're dialing we'd want to reach out to any of these closer to that $tap_end
				# also check to see if we want them in a bucket
				if(bucket_want($seeipp,$buckets))
				{
					# send direct (should open our outgoing to them)
					my $jo = tnew($seeipp);
					$jo->{"+end"} = $selfhash;
					tsend($jo);
					# send pop signal back to the switch who .see'd us in case the new one is behind a nat
					my $jo = tnew($remoteipp);
					$jo->{"+end"} = sha1_hex($seeipp);
					$jo->{"+pop"} = "th:$selfipp";
					$jo->{"_hop"} = 1;
					tsend($jo);
				}
			}
		}

		# handle a tap command, add/replace rules
		if($j->{".tap"} && ref $j->{".tap"} eq "ARRAY")
		{
			$line->{"rules"} = $j->{".tap"};
		}
	}

	# no open line, or after we processed commands, we can process any signals
	if(grep(/^\+.+/,keys %$j))
	{
		# any first-hop end signal is responded to
		if($j->{"+end"} && int($j->{"_hop"}) == 0)
		{
			my $vis = $line->{visible} ? $remoteipp : $selfipp; # start from a visible switch (should use cached result someday)
			my @hashes = near_to($j->{"+end"},$vis); # get closest hashes (of other switches)
			my %ipps = map {$master{$_}->{ipp} => 1} perlsucks(5,\@hashes); # convert back to IPPs
			# TODO: this is where dampening should happen to not advertise switches that might be too busy
			$ipps{$selfipp}=$line->{visibled}=1 unless($line->{visibled}); # mark ourselves visible at least once
			
			if(scalar keys %ipps > 0)
			{
				my $jo = tnew($remoteipp);
				my @ipps = grep { length($_) > 1 } keys %ipps;
				$jo->{".see"} = \@ipps;
				tsend($jo);
			}
		}
		
		# this is our .tap, requests to +pop for NATs
		if($j->{"+end"} eq $selfhash && $j->{"+pop"} =~ /th\:([\d\.]+)\:(\d+)/)
		{ # should we verify that this came from a switch we actually have a tap on?
			my $ip = $1;
			my $port = $2;
			printf STDERR "POP to $ip:$port\n";
			tsend(tnew("$ip:$port"));
		}

		# if not last-hop, check for any active taps (todo: optimize the matching, this is just brute force)
		if(int($j->{"_hop"}) < 4)
		{
			for my $hash (grep($master{$_}->{rules},keys %master))
			{
				my $pass=0;
				my $swipp = $master{$hash}->{ipp};
				for my $rule (@{$master{$hash}->{"rules"}})
				{
					printf STDERR "\tTAP CHECK IS %s\t%s\n",$swipp,$json->to_json($rule);
					# all the "is" are in this telex and match exactly
					next unless(scalar grep($j->{$_} eq $rule->{"is"}->{$_}, keys %{$rule->{"is"}}) == scalar keys %{$rule->{"is"}});
					# pass only if all has exist
					my $haspass=1;
					for my $sig (@{$rule->{"has"}})
					{
						$haspass=0 unless($j->{$sig});
					}
					$pass++ if($haspass);
				}
				# forward this switch a copy
				if($pass)
				{
					# it's us, it has to be our tap_js        
					if($swipp eq $selfipp)
					{
						printf STDERR "STDOUT[%s]\n",$json->to_json($j);
						printf "%s\n",$json->to_json($j);
					}else{
						my $jo = tnew($swipp);
						for my $sig (grep(/^\+.+/, keys %$j))
						{
							$jo->{$sig} = $j->{$sig};
						}
						$jo->{"_hop"} = int($j->{"_hop"})+1;
						tsend($jo);
					}
				}else{
					printf STDERR "\tCHECK MISS\n";
				}
			}
		}
	}
}

# for creating and tracking lines to switches
sub getline
{
	my $switch = shift;
	return undef if(length($switch) < 4); # sanity
	my $hash = sha1_hex($switch);
	if(!$master{$hash} || $master{$hash}->{ipp} ne $switch)
	{
		printf STDERR "\tNEWLINE[%s]\n",$switch;
		my($ip,$port) = split(":",$switch);
		my $wip = gethostbyname($ip);
		return undef unless($ip && $wip); # bad ip?
		my $addr = sockaddr_in($port,$wip);
		return undef unless($port && $addr); # bad port?
		$master{$hash} = { ipp=>$switch, end=>$hash, addr=>$addr, ringout=>int(rand(32768))+1, init=>time(), seenat=>0, sentat=>0, lineat=>0, br=>0, brout=>0, brin=>0, bsent=>0, neighbors=>{$hash=>1}, visible=>0 };
	}
	return $master{$hash};
}

sub perlsucks
{
	# why does my brain hurt, gawd
	my $cnt = shift;
	my $ar = shift;
	my @a = @$ar;
	my $acnt = scalar @a;
	my @ret = grep {$_} splice(@a,0,$cnt);
	return @ret;
}

# generate a .see for an +end, using a switch as a hint
sub near_to
{
	my $end = shift;
	my $ipp = shift;
	my $line = $master{sha1_hex($ipp)};
	return undef unless($line && $line->{ipp} eq $ipp); # should always exist except in startup or offline, etc

	# of the existing and visible cached neighbors, sort by distance to this end
	my @see = sort {hash_distance($end,$a)<=>hash_distance($end,$b)} grep {$master{$_} && $master{$_}->{visible}} keys %{$line->{neighbors}};

	printf STDERR "\t\tNEARTO %s\t%s=%s\t%s>>>%s\t%d=%d\n",$end,$ipp,$line->{end},join(",",keys %{$line->{neighbors}}),join(",", @see),hash_distance($see[0],$end),hash_distance($see[0],$line->{end});

	return undef unless(scalar @see);

	# it's either us or we're the same distance away so return these results
	if($see[0] eq $line->{end} || hash_distance($see[0],$end) == hash_distance($see[0],$line->{end}))
	{
		# this +end == this line then replace the neighbors cache with this result and each in the result walk and insert self into their neighbors
		if($line->{end} eq $end)
		{
			printf STDERR "\t\tNEIGH for %s was %s %d\n",$end,join(",",keys %{$line->{neighbors}}),scalar @see;
			my %neigh = map {$_=>1} perlsucks(5,\@see);
			$line->{neighbors} = \%neigh;
			printf STDERR "\t\tNEIGH for %s is %s %d\n",$end,join(",",keys %{$line->{neighbors}}),scalar @see;
			for my $hash (keys %{$line->{neighbors}})
			{
				$master{$hash}->{neighbors}->{$end}=1;
				printf STDERR "\t\tSEED %s into %s\n",$ipp,$master{$hash}->{ipp};
			}
		}
		printf STDERR "\t\t\tSEE distance=%d count=%d\n",hash_distance($end,$see[0]),scalar @see;
		return @see;
	}

	# whomever is closer, if any, tail recurse endseeswitch them
	return near_to($end,$master{$see[0]}->{ipp});
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
		}
	}

	# we're valid at this point, line or otherwise, track bytes
	printf STDERR "\tBR %s [%d += %d] DIFF %d\n",$line->{ipp},$line->{br},$br,$line->{bsent} - $t->{_br};
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
		printf STDERR "\tMAX SEND DROP\n";
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
	printf STDERR "SEND[%s]\t%s\n",$j->{"_to"},$js;
	if(!defined(send(SOCKET, $js, 0, $line->{"addr"})))
	{
		offline("tsend");
		return;
	}
	$line->{"bsent"} += length($js);
	$line->{"sentat"} = time();
}

sub offline
{
	$selfipp=$connected=undef;
	%master = ();
	printf STDERR "\tOFFLINE %s\n",shift;	

}

sub taptap
{
	# todo, use last nearest as cached from/for near_to, or $selfipp if it's gone
	my @hashes = near_to($tap_end,$selfipp); # get closest hashes (of other switches)
	printf STDERR "TAPTAP from %d to %s end %s tap %s\n",scalar @hashes,$master{$hashes[0]}->{ipp},$tap_end,$json->to_json($tap_js);
	return unless(scalar @hashes > 1);
	# tap the top 3 closest
	for my $hash (perlsucks(3,\@hashes))
	{
		next if($hash eq $selfhash);
		my $line = $master{$hash};
		return if($line->{taplast} && $line->{taplast}+50 > time()); # only tap every 50sec
		$line->{taplast} = time();
		my $jo = tnew($line->{ipp}); # tap the closest ipp to our target end 
		$jo->{".tap"} = $tap_js;
		# todo someday, include test signals and check to make sure they're returned or flag ignore this switch
		tsend($jo);
	}
}

# scan all known switches to keep any nat's open
my $lastscan;
sub scanlines
{
	my $at = time();
	return if($lastscan+30 > $at);
	$lastscan=$at;
	my @switches = keys %master;
	my $valid=0;
	printf STDERR "SCAN\t%d\n",scalar @switches;
	for my $hash (@switches)
	{
		next if($hash eq $selfhash || length($hash) < 10); # ??
		my $line = $master{$hash};
		next if($line->{"end"} ne $hash); # empty/dead line
		if(($line->{"seenat"} == 0 && $at - $line->{"init"} > 70) || ($line->{"seenat"} != 0 && $at - $line->{"seenat"} > 70))
		{ # remove them if they never responded or haven't in a while
			printf STDERR "\tPURGE[%s %s] last seen %d ago\n",$hash,$line->{ipp},$at - $line->{seenat};
			$master{$hash} = {};
			next;
		}
		$valid++;
		# end ourselves to see if they know anyone closer as a ping
		my $jo = tnew($line->{ipp});
		$jo->{"+end"} = $selfhash;
		# also .see ourselves if we haven't yet, default for now is to participate in the DHT
		$jo->{".see"} = [$selfipp] unless($line->{visibled}++);
		# also .tap our hash for +pop requests for NATs
		$jo->{".tap"} = [{"is"=>{"+end"=>$selfhash},"has"=>["+pop"]}];
		tsend($jo);
	}
	# if no lines and we're not the seed
	offline("scan") if($valid == 0 && $selfipp ne $seedipp);
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
	printf STDERR "SEEDING[%s]\n",$seed;
	my $line = getline($seed);
	$line->{"bsent"}=0; # always reset for seeding
	my $jo = tnew($seed);
	$jo->{"+end"} = $line->{end}; # any end will do, might as well ask for their neighborhood
	tsend($jo);
}

# see if we want to try this switch or not, and maybe prune the bucket
sub bucket_want
{
	my $ipp = shift;
	my $buckets = shift;
	my $pos = hash_distance(sha1_hex($ipp),$selfhash);
	printf STDERR "\tBUCKET WANT[%d %s]\n",$pos,$ipp;
	return undef if($pos < 0 || $pos > 159); # err!?
	return 1; # for now we're always taking everyone, in future need to be more selective when the bucket is "full"!
}

# returns xor distance between two sha1 hex hashes, 159 is furthest bit, 0 is closest bit, -1 is same hash
sub hash_distance
{
	my $ha = shift;
	my $hb = shift;
	my @a = map {hex $_} split undef,$ha;
	my @b = map {hex $_} split undef,$hb;
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
