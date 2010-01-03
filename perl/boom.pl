use Socket;

my $port = $ARGV[0]||0;

$iaddr = gethostbyname("0.0.0.0");
$proto = getprotobyname('udp');
$paddr = sockaddr_in($port, $iaddr); # 0 means let kernel pick
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)   or die "socket: $!";
bind(SOCKET, $paddr)                          or die "bind: $!";

defined(send(SOCKET, "1", 0, $paddr))    or die "$!";

my $buff;
while(recv(SOCKET, $buff, 8192, 0))
{
	printf "%s\n",$buff;
	$buff++;
	defined(send(SOCKET, $buff, 0, $paddr))    or die "$!";
	defined(send(SOCKET, $buff, 0, $paddr))    or die "$!";
}
die "recv: $!";

