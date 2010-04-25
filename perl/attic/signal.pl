#!/usr/bin/perl

# given a writer ip:port, end hash, signal name and value, just send it

use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $ipp = $ARGV[0];
my $end = $ARGV[1];
my $sig = $ARGV[2];
my $val = $ARGV[3] || die("./signal.pl ip:port hashcode signame sigvalue");

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

my $j = {"_to"=>$ipp, "end"=>$end, $sig=>$val};
my($ip,$port) = split(":",$ipp);
my $wip = gethostbyname($ip);
my $waddr = sockaddr_in($port,$wip);
my $js = $json->to_json($j);
printf "SEND[%s]\t%s\n",$j->{"_to"},$js;
defined(send(SOCKET, $js, 0, $waddr))    or die "send $to: $!";	

