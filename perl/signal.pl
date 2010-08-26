#!/usr/bin/perl

# given a writer ip:port, end hash, signal name and value, just send it

use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $ipp = $ARGV[0];
my $end = $ARGV[1];
$end = (length($end)==40)?$end:sha1_hex($end);
my $sig = $ARGV[2];
my $val = $ARGV[3] || die("./signal.pl ip:port endhash-or-string signame sigvalue");

# reset just in case it was a hostname
my($ippi,$ippp) = split(":",$ipp);
$ipp = sprintf("%s:%d",inet_ntoa(scalar gethostbyname($ippi)),$ippp);

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

my $j = {"_to"=>$ipp, "+end"=>$end, $sig=>$val, "_hop"=>1}; # hop of one implies no .see reply needed
my($ip,$port) = split(":",$ipp);
my $wip = gethostbyname($ip);
my $waddr = sockaddr_in($port,$wip);
my $js = $json->to_json($j);
printf "SEND[%s]\t%s\n",$j->{"_to"},$js;
defined(send(SOCKET, $js, 0, $waddr))    or die "send $to: $!";	

