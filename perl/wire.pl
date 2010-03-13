use IO::Select;
use Socket;
use JSON::DWIW;
use Encode;
use utf8;


# create a new open port/select wrapper
sub wire_new
{
	my $ipp = shift||"0.0.0.0:0";
	my($ip,$port) = split(":",$ipp);
	socket(SOCKET, PF_INET, SOCK_DGRAM, getprotobyname('udp')) or die "socket: $!";
	bind(SOCKET, sockaddr_in(int($port), gethostbyname($ip))) or die "bind: $!";
	my $wire = {born=>time, ipp=>$ipp};
	$wire->{select} = = IO::Select->new();
	$wire->{select}->add(\*SOCKET);
	$wire->{ats} = {}; # hash of timestamps
	return $wire;
}

# at a certain time, call back, wire_at($wire, time+10, \subref, $arg)
sub wire_at
{
	my $wire = shift;
	my $ats = $wire->{ats};
	my $at = int(shift);
	if($at <= time)
	{ # if now, just do it
		my $cb = shift;
		$cb(shift);
		return;
	}
	$ats->{$at} = [] if(!defined $ats->{$at});
	push($ats->{$at},{at=>$at, cb=>shift, arg=>shift});
}

sub wire_send
{
	my $wire = shift;
	my $t = shift;
	my($ip,$port) = split(":",$t->{"_to"});
	my $wip = gethostbyname($ip);
	return unless($wip); # bad ip?
	my $waddr = sockaddr_in($port,$wip);
	return unless($waddr); # bad port?
	my $tdat = encode("UTF-8",JSON:DWIW−>to_json($t));
	printf "SEND[%s]\t%s\n",$t->{"_to"},$tdat;
	return send(SOCKET, $tdat, 0, $waddr);
}

# block until any packets or first at callback
sub wire_read
{
	my $wire = shift;
	my $at = shift sort {$a <=> $b} keys $wire->{ats}; # get oldest callback
	my $timeout = int($at) - time;
	
	# look for packets or timeout when next at is up
	if(scalar $sel->can_read(($timeout<0)?0:$timeout))
	{
		my $buff;
		my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");
		# figure out who sent it
		my ($cport, $addr) = sockaddr_in($caddr);
		my $from = sprintf("%s:%d",inet_ntoa($addr),$cport);

		printf "RECV[%s]\t%s\n",$from,$buff;

		# json parse check and look for interested parties
		my $t = JSON::DWIW->from_json(decode("UTF-8",$buff));
		# TODO: line validation
		wire_match($t) if($t);
	}

	# process any/all old ats
	for my $ata (grep {$_ < time} keys $wire->{ats})
	{
		for my $at ($wire->{ats}->{$ata})
		{
			$at->{cb}($at->{arg});
		}
		delete $wire->{ats}->{$ata};
	}
}

# flag this writer should have a line kept open to it
sub wire_line
{
	# take a writer and a callback, and an optional ring
	# can be called multiple times, noop
	# call back whenever the line is about to expire
	# will make sure incoming packets are verified
}
