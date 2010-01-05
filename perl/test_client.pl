#!/usr/bin/perl

use Digest::SHA1 qw(sha1_hex);
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

# create our local UDP listener port
my $iaddr = gethostbyname("0.0.0.0");
my $proto = getprotobyname('udp');
my $paddr = sockaddr_in(0, $iaddr); # pick local port at random
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)	or die "socket: $!";
bind(SOCKET, $paddr)						or die "bind: $!";

# send a hello to our seed
my $seed = $ARGV[0]||"telehash.org:42424";
my($ip,$port) = split(":",$seed);
my $sip = gethostbyname($ip);
my $saddr = sockaddr_in($port,$sip);
defined(send(SOCKET, "{}", 0, $saddr))		or die "hello failed to $seed: $!";

# get the first response and validate
my $buff;
my $caddr = recv(SOCKET, $buff, 8192, 0);
my($cport, $addr) = sockaddr_in($caddr);
my $sender = sprintf("%s:%d",inet_ntoa($addr),$cport);
my $j = $json->from_json($buff)				or die("json parse failed: $buff");
defined($j->{"_cb"} eq $sender)				or die("seed source $sender disagrees with it's callback ".$j->{"_cb"});
defined($j->{".cb"})						or die("first response was missing a callback command");

printf "%s told us we are %s\n",$j->{"_cb"},$j->{".cb"};

# quite temporary
require "./bixor.pl";
printf "our distance from the seed is %d\n",bix_sbit(bix_or(bix_new(sha1_hex($j->{"_cb"})),bix_new(sha1_hex($j->{".cb"}))));