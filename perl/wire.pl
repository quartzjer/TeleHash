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
	$wire->{lines} = {}; # lines active, key is ipp
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
	# TODO: look for a line and set vars if so, update sentat
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
		# TODO: line validation, update seenat
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

# flag this writer should have a line kept open to it (with optional ring), call back if it's going to expire or failed
#	wire_line($wire,"1.2.3.4:5678",910,cb_check,cb_fail,arg)
sub wire_line
{
	my ($wire,$ipp,$ringin,$cb,$arg) = shift;
	if(!$wire->{lines}->{$ipp})
	{ # create a new line
		my $ringout = int(rand(32768));
		$wire->{lines}->{$ipp} = {wire=>$wire, ringin=>$ringin, ringout=>$ringout, line=>($ringin*$ringout), callbacks=>{}, seenat=>0, sentat=>0};
	}
	my $line = $wire->{lines}->{$ipp};
	
	# can be called multiple times, noop
	return if($line->{callbacks}->{$cb});

	# save this callback and set up to call it right away
	$line->{callbacks}->{$cb} = $arg;
	wire_at($wire,time+5,wire_line_check,$line);
}

# maintenance on lines
sub wire_line_check
{
	my $line = shift;

	# first check if the line has been active, then defer the check
	my $age = time - $line->{seenat};
	if($age < 50)
	{
		wire_at($line->{wire},55-$age,wire_line_check,$line);
		return;
	}
	
	# notify the callback(s), delete ourselves if none care anymore
	# TODO
}
