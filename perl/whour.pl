#!/usr/bin/perl
use Digest::SHA1 qw(sha1_hex);
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $ipp = $ARGV[0]||die("need IP:PORT");
my($ip,$port) = split(":",$ipp);

$iaddr = gethostbyname($ip);
$proto = getprotobyname('udp');
$paddr = sockaddr_in($port, $iaddr);
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)   or die "socket: $!";
bind(SOCKET, $paddr)                          or die "bind: $!";

my %cache; # just a dumb cache
require "./bixor.pl"; # temp testing hack
my $buff;
while(my $caddr = recv(SOCKET, $buff, 8192, 0))
{
	# need some source rate detection in case there's a loop
	($cport, $addr) = sockaddr_in($caddr);
	my $cb = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "got %s from %s\n",$buff,$cb;
	my $j = $json->from_json($buff) || next;
	$cache{sha1_hex($cb)}=$cb; # track all seen writers for .to/.see testing
	if(!$j->{"_cb"})
	{ # all we do is reply to missing _cb's
		my $jo = { "_cb"=>$ipp, ".cb"=>$cb };
    	defined(send(SOCKET, $json->to_json($jo), 0, $caddr))    or die "send $cb $!";
		next;
	}
	if($j->{".to"})
	{
		my $bto = bix_new($j->{".to"}); # convert to format for the big xor for faster sorting
		my @ckeys = sort {bix_sbit(bix_or($bto,bix_new($a))) <=> bix_sbit(bix_or($bto,bix_new($b)))} keys %cache; # sort by closest to the .to
		printf("from %d writers, closest is %d\n",scalar @ckeys, bix_sbit(bix_or($bto,bix_new($ckeys[1]))));
		my @cipps = map {$cache{$_}} splice @ckeys, 0, 5; # just take top 5 closest
		my $jo = { "_cb"=>$ipp, ".see"=>\@cipps }; 
    	defined(send(SOCKET, $json->to_json($jo), 0, $caddr))    or die ".see $cb $!";
		next;
	}
	if($j->{".natr"})
	{
		my($ip,$port) = split(":",$j->{".natr"});
		my $nip = gethostbyname($ip);
		my $naddr = sockaddr_in($port,$nip);
		my $jo = { "_cb"=>$ipp, ".nat"=>$j->{"_cb"} }; 
    	defined(send(SOCKET, $json->to_json($jo), 0, $naddr))    or die ".nat $ip:$port $!";
		next;
	}
}
die "recv: $!";

