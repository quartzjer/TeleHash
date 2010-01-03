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

my $buff;
while(my $caddr = recv(SOCKET, $buff, 8192, 0))
{
	# need some source rate detection in case there's a loop
	($cport, $addr) = sockaddr_in($caddr);
	my $cb = sprintf("%s:%d",inet_ntoa($addr),$cport);
	printf "got %s from %s\n",$buff,$cb;
	my $j = $json->from_json($buff) || next;
	next if($j->{"_cb"}); # all we do is reply to missing _cb's
	my $jo = { "_cb"=>$ipp, ".cb"=>$cb };
    defined(send(SOCKET, $json->to_json($jo), 0, $caddr))    or die "send $cb $!";
}
die "recv: $!";

