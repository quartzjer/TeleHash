#!/usr/bin/perl

# given a writer ip:port, end hash, signal name, and count, request any historical telexes for it

use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $ipp = $ARGV[0];
my $sig = $ARGV[1] || die("./history.pl ip:port signame count endhash");
my $cnt = $ARGV[2] || 10;
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

my $j = {"_to"=>$ipp, ".hist"=>{$sig=>$cnt}};
$j->{"end"} = $end if($end);
my($ip,$port) = split(":",$ipp);
my $wip = gethostbyname($ip);
my $waddr = sockaddr_in($port,$wip);
my $js = $json->to_json($j);
printf "SEND[%s]\t%s\n",$j->{"_to"},$js;
defined(send(SOCKET, $js, 0, $waddr))    or die "send $to: $!";	

while(1)
{
	# wait for event or timeout loop
	if(scalar $sel->can_read(5) == 0)
	{
		exit();
	}

	# must be a telex waiting for us
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");

	# figure out who sent it
	($cport, $addr) = sockaddr_in($caddr);
	my $writer = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "RECV[%s]\t%s\n",$writer,$buff;
}
