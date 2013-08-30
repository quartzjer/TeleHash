#!/usr/bin/perl

# given a writer ip:port, end hash, signal name and value, just send it

# TODO: doesn't support writers behind a nat yet

use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $ipp = $ARGV[0] || die("./listen.pl ip:port [signal] [endhash or string]");
my $sig = $ARGV[1];
my $end = $ARGV[2];
$end = (length($end)==40)?$end:sha1_hex($end) if($end);


# reset just in case it was a hostname
my($ippi,$ippp) = split(":",$ipp);
$ipp = sprintf("%s:%d",inet_ntoa(scalar gethostbyname($ippi)),$ippp);

# defaults to listen on any ip and random port
my $port = 0;
my $ip = "0.0.0.0"; 
my $br = 0;
my $line = 0;

$iaddr = gethostbyname($ip);
$proto = getprotobyname('udp');
$paddr = sockaddr_in($port, $iaddr);
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)   or die "socket: $!";
bind(SOCKET, $paddr)                          or die "bind: $!";
$sel = IO::Select->new();
$sel->add(\*SOCKET);

# send initial hello to open line
my $jo = telex($ipp);
$jo->{"+end"}=sha1_hex($ipp);
tsend($jo);

my $regd;
my $lastloop=time();
while(1)
{
	# wait for event or timeout loop
	if(scalar $sel->can_read(50) == 0)
	{
		next;
	}

	# wait for event or timeout loop
	my $newmsg = scalar $sel->can_read(10);
	if($newmsg == 0 || $lastloop+45 < int(time))
	{
		tsend(telex($ipp)); # send keepalive
		$lastloop = int(time);
		next if($newmsg == 0); # timeout loop
	}

	# must be a telex waiting for us
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");
	# TODO need some source rate detection in case there's a loop

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $remoteipp = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "RECV[%s]\t%s\n",$remoteipp,$buff;
	if($remoteipp ne $ipp)
	{
		printf "NOTHANKS\n";
		next;
	}

	# json parse check
	my $j = $json->from_json($buff) || next;

	if(!$j->{"_line"} && !$j->{"_ring"})
	{
		printf "LINEMISSING\n";
		next;
	}

	# track bytes received so we can keep getting more forwards
	$br += length($buff);

	# first time they respond at all, send them the tap request now that we have a _line to validate it
	if(!$regd)
	{
		$regd++;
		my $jo = telex($ipp);
		$jo->{".tap"} = ();
		$jo->{".tap"}->[0] = {"has"=>[$sig]};
		$jo->{".tap"}->[0]->{"is"} = {"+end"=>$end} if($end);
		$line = int($j->{"_ring"});
		tsend($jo);
	}

}

# create a new telex
sub telex
{
	my $to = shift;
	my $js = shift || {};
	$js->{"_to"} = $to;
	return $js;
}

# actually send a telex to its writer
sub tsend
{
	my $j = shift;
	my($ip,$port) = split(":",$j->{"_to"});
	my $wip = gethostbyname($ip);
	my $waddr = sockaddr_in($port,$wip);
	$j->{"_br"} = $br;
	$j->{"_line"} = $line if($line > 0);
	my $js = $json->to_json($j);
	printf "SEND[%s]\t%s\n",$j->{"_to"},$js;
	defined(send(SOCKET, $js, 0, $waddr))    or die "send $to: $!";	
}
