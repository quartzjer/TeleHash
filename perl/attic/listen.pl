#!/usr/bin/perl

# given a writer ip:port, end hash, signal name and value, just send it

# TODO: doesn't support writers behind a nat yet

use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $ipp = $ARGV[0];
my $sig = $ARGV[1];
my $cnt = $ARGV[2] || die("./listen.pl ip:port signal count hashcode");
my $end = $ARGV[3];

# defaults to listen on any ip and random port
my $port = 0;
my $ip = "0.0.0.0"; 

$iaddr = gethostbyname($ip);
$proto = getprotobyname('udp');
$paddr = sockaddr_in($port, $iaddr);
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)   or die "socket: $!";
bind(SOCKET, $paddr)                          or die "bind: $!";
$sel = IO::Select->new();
$sel->add(\*SOCKET);

# send initial hello to open line
my $jo = telex($ipp);
$jo->{"end"}=sha1_hex($ipp);
tsend($jo);

my $regd;
while(1)
{
	# wait for event or timeout loop
	if(scalar $sel->can_read(50) == 0)
	{
		tsend(telex($ipp)); # send keepalive
		next;
	}

	# must be a telex waiting for us
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");
	# TODO need some source rate detection in case there's a loop

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $writer = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "RECV[%s]\t%s\n",$writer,$buff;
	if($writer ne $ipp)
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
	
	# first time they respond at all, send them the fwd request now that we have a _line to validate it
	if(!$regd)
	{
		$regd++;
		my $jo = telex($ipp);
		$jo->{".fwd"} = {$sig=>$cnt};
		$jo->{"end"} = $end if($end);
		$jo->{"_line"} = $j->{"_ring"};
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
	my $js = $json->to_json($j);
	printf "SEND[%s]\t%s\n",$j->{"_to"},$js;
	defined(send(SOCKET, $js, 0, $waddr))    or die "send $to: $!";	
}
