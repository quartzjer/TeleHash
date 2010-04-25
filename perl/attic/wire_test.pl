require "./wire.pl";
use Data::Dumper;

my $wire = wire_new();
print Dumper($wire);
wire_at($wire,time+5,\&testcb,"foo");
wire_read($wire);
sub testcb
{
	printf "CB[%s]\n",shift;
}