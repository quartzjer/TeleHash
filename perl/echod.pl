use Socket;
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $port = $ARGV[0]||0;

$iaddr = gethostbyname("0.0.0.0");
$proto = getprotobyname('udp');
$paddr = sockaddr_in($port, $iaddr); # 0 means let kernel pick
socket(SOCKET, PF_INET, SOCK_DGRAM, $proto)   or die "socket: $!";
bind(SOCKET, $paddr)                          or die "bind: $!";

my $buff;
while($paddr = recv(SOCKET, $buff, 8192, 0))
{
	($port, $addr) = sockaddr_in($paddr);
	printf "got %s from %s:%d\n",$buff,inet_ntoa($addr),$port;
	my $j = $json->from_json($buff) || next;
	my $jo = {};
	if(!$j->{"_cb"})
	{
		$jo->{".cb"} = sprintf("%s:%d",inet_ntoa($addr),$port);
	}
    defined(send(SOCKET, $json->to_json($jo), 0, $paddr))    or die "send $hisiaddr:$port $!";
}
die "recv: $!";

