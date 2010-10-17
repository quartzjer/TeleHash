#!/usr/bin/perl

# given a name of a wall, sit and listen to anything coming from it

die("temp hack, plz run from the same dir as switch.pl") if(!-f "switch.pl");
my $wall = $ARGV[0]||"42";
my $doit = $ARGV[1];
my $end = sha1_hex($wall);
my $tap = "[{\"is\":{\"+end\":\"$end\"},\"has\":[\"+wall\"]}]";
my %dedup;

use IO::Handle;
use IPC::Open2;
use Digest::SHA1 qw(sha1_hex);
use JSON::DWIW;
my $json = JSON::DWIW->new;

my $run = sprintf("./switch.pl -e '%s' -t '%s' |",$end,$tap);
printf "running %s\n",$run;
my ($read_fh, $write_fh) = (IO::Handle->new(), IO::Handle->new());
$pid = open2($read_fh, $write_fh, "./switch.pl","-e",$end,"-t",$tap)||die("oops $!");
while(<$read_fh>)
{
	chop;
	my $js = $json->from_json($_);
	my $wall = $js->{"+wall"};
	next if($js->{"+guid"} && $dedup{$js->{"+guid"}}++);
	if($doit)
	{
		open(R,"|$doit");
		print R $wall."\n";
		close(R);
	}
	printf "%s\t%s\n",`date`,$wall;		
}
close($read_fh);
close($write_fh);
