use IO::Select;
use Socket;
use JSON::DWIW;
use Encode;
use utf8;
use Data::Dumper;


# create a new open port/select wrapper
sub wire_new
{
	my $ipp = shift||"0.0.0.0:0";
	my($ip,$port) = split(":",$ipp);
	socket(SOCKET, PF_INET, SOCK_DGRAM, getprotobyname('udp')) or die "socket: $!";
	my $iaddr = gethostbyname($ip);
	bind(SOCKET, sockaddr_in(int($port), $iaddr)) or die "bind: $!";
	my $wire = {born=>time, ipp=>$ipp};
	$wire->{select} = IO::Select->new();
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
		&$cb(shift);
		return;
	}
	$ats->{$at} = [] if(!defined $ats->{$at});
	push(@{$ats->{$at}},{at=>$at, cb=>shift, arg=>shift});
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

	# get/update our line stats and line vars in the telex
	my $line = wire_getline($wire,$t->{"_to"});
	$line->{sentat} = time;
	if($line->{line} > 0)
	{
		$t->{"_line"} = $line->{line};
	}else{
		$t->{"_ring"} = $line->{ringout};
	}
	$t->{"_limbo"} = $line->{limbo};
	my $tdat = encode("UTF-8",JSON::DWIW->to_json($t));
	$line->{limbo} += length($tdat);
	printf "SEND[%s]\t%s\n",$t->{"_to"},$tdat;
	return send(SOCKET, $tdat, 0, $waddr);
}

# block until any packets or first at callback
sub wire_read
{
	my $wire = shift;
	my @ats = sort {$a <=> $b} keys %{$wire->{ats}}; # get oldest callback
	my $at = shift @ats||time+600;
	my $timeout = int($at) - time;
	
	# look for packets or timeout when next at is up
	if(scalar $wire->{select}->can_read(($timeout<0)?0:$timeout))
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
	for my $ata (grep {$_ <= time} keys %{$wire->{ats}})
	{
		for my $at ($wire->{ats}->{$ata})
		{
			&{$at->{cb}}($at->{arg});
		}
		delete $wire->{ats}->{$ata};
	}
}

# internal to init/return line for any ipp, optional incoming ring if known
sub wire_getline
{
	my ($wire,$ipp,$ring) = shift;
	my $line = $wire->{lines}->{$ipp};
	if(!$line)
	{ # create a new line, set up timer to watch/expire it if inactive
		$line = $wire->{lines}->{$ipp} = {wire=>$wire, limbo=>0, ringout=>int(rand(32768)+1), callbacks=>{}, seenat=>0, sentat=>0};
		wire_at($wire,time+51,wire_line_check,$line);
	}
	if($ring > 0 && $ring <= 32768)
	{ # line is open!
		$line->{ringin} = $ring;
		$line->{line} = $line->{ringin} * $line->{ringout};
	}
	return $line;
}

# flag this writer should have a line kept open to it with call back if it's going to expire or failed
#	wire_line($wire,"1.2.3.4:5678",callback,arg)
sub wire_line
{
	my ($wire,$ipp,$cb,$arg) = shift;
	my $line = wire_getline($wire,$ipp);
	
	# can be called multiple times, noop
	return if($line->{callbacks}->{$cb});

	# save this callback, it'll get called during line_check/expire
	$line->{callbacks}->{$cb} = $arg;
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
	# if anyone cares, .end ourselves to them and set wire_line_expire in 10sec
	# if nobody cares, just call wire_line_expire
}

# maintenance on lines
sub wire_line_expire
{
	my $line = shift;

	# if it's been active again, go back to check mode!
	my $age = time - $line->{seenat};
	if($age < 50)
	{
		wire_at($line->{wire},55-$age,wire_line_check,$line);
		return;
	}
	
	# notify the callback(s) of the expiration
	# delete ourselves
}

1;