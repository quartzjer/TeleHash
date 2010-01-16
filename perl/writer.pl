#!/usr/bin/perl
use Digest::SHA1 qw(sha1_hex);
use IO::Select;
use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

# defaults to listen on any ip and random port
my $port = $ARGV[0]||0;
my $ip = "0.0.0.0"; 
my $seed = $ARGV[1]||"telehash.org:42424";

$iaddr = gethostbyname($ip);
$proto = getprotobyname('udp');
$paddr = sockaddr_in($port, $iaddr);
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)   or die "socket: $!";
bind(SOCKET, $paddr)                          or die "bind: $!";
$sel = IO::Select->new();
$sel->add(\*SOCKET);

# send a hello to our seed
my($seedhost,$seedport) = split(":",$seed);
my $seedip = gethostbyname($seedhost);
my $seedipp = sprintf("%s:%d",inet_ntoa($seedip),$seedport);
my $saddr = sockaddr_in($seedport,$seedip);
my $jo = telex($seedipp);
$jo->{".end"} = sha1_hex(rand()); # random end, just to provoke a .see that has a _to to identify ourselves
defined(send(SOCKET, $json->to_json($jo), 0, $saddr))		or die "hello failed to $seed: $!";

my %cache; # just a dumb cache
my %lines; # static line assignments per writer
require "./bixor.pl"; # temp testing hack
my $buff;
$|++;
my $ipp;
while(1)
{
	# wait for event or timeout loop
	if(scalar $sel->can_read(10) == 0)
	{
		print ".";
		next;
	}
	my $caddr = recv(SOCKET, $buff, 8192, 0) || die("recv $!");
	# TODO need some source rate detection in case there's a loop
	($cport, $addr) = sockaddr_in($caddr);
	my $cb = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "\n%s: %s\n",$cb,$buff;
	my $j = $json->from_json($buff) || next;
	$cache{sha1_hex($cb)}=$cb; # track all seen writers for .end/.see testing
	$ipp = $j->{"_to"} if(!$ipp && $j->{"_to"}); # discover our own ip:port
	if($j->{".end"})
	{
		my $bto = bix_new($j->{".end"}); # convert to format for the big xor for faster sorting
		my @ckeys = sort {bix_sbit(bix_or($bto,bix_new($a))) <=> bix_sbit(bix_or($bto,bix_new($b)))} keys %cache; # sort by closest to the .end
		printf("from %d writers, closest is %d\n",scalar @ckeys, bix_sbit(bix_or($bto,bix_new($ckeys[1]))));
		my @cipps = map {$cache{$_}} splice @ckeys, 0, 5; # just take top 5 closest
		my $jo = telex($cb);
		$jo->{".see"} = \@cipps; 
    	defined(send(SOCKET, $json->to_json($jo), 0, $caddr))    or die ".see $cb $!";
		next;
	}
	if($j->{".natr"})
	{
		my($ip,$port) = split(":",$j->{".natr"});
		my $nip = gethostbyname($ip);
		my $naddr = sockaddr_in($port,$nip);
		my $jo = telex($cb);
		$jo->{".nat"} = $cb; 
    	defined(send(SOCKET, $json->to_json($jo), 0, $naddr))    or die ".nat $ip:$port $!";
		next;
	}
}

sub telex
{
	my $to = shift;
	my $js = shift || {};
	$lines{$to} = int(rand(65535)) unless($lines{$to}); # assign a line for this recipient just once
	$js->{"_to"} = $to;
	$js->{"_line"} = $lines{$to};
	return $js;
}
