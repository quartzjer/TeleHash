#!/usr/bin/perl
#use Data::Dumper;

# big xor utils
# each bix is an array of 20 chars

#my $in = $ARGV[0];
#my $b = bix_new($in);
#my $b2 = bix_new($ARGV[1]);
#my $out = bix_str($b);
#printf "got %s converted to %s and xor'd %s and sbit %d\n",$in,$out,bix_str(bix_or($b,$b2)),bix_sbit(bix_or($b,$b2));

# takes hex string
sub bix_new
{
	my @bix;
	for my $b (split undef,shift)
	{
		push @bix,hex $b;
	}
	return \@bix;
}
sub bix_str
{
	my $br = shift;
	my $str;
	for (@$br)
	{
		$str .= sprintf "%x",$_;
	}
	return $str;
}
sub bix_or
{
	my $a = shift;
	my $b = shift;
	my @c;
	for my $i (0..39)
	{
		$c[$i] = $a->[$i] ^ $b->[$i];
	}
	return \@c;
}

# invert the bits, or make a hash as far away as possible
sub bix_far
{
	my $a = shift;
	my @c;
	for my $i (0..39)
	{
		$c[$i] = $a->[$i] ^ hex 'f';
	}
	print "\n";
	return \@c;
}

sub bix_sbit
{
	my $b = shift;
	my @sbtab = (-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3); # ln(hex)/log(2)
	my $ret = 156;
	for my $i (0..39)
	{
		return $ret + $sbtab[$b->[$i]] if($b->[$i]);
		$ret -= 4;
	}
	return -1;
}
1;