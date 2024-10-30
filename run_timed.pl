#!/usr/bin/env perl
use strict;
use warnings;

use Time::HiRes qw(gettimeofday tv_interval);
use Data::Dumper;
use List::Util qw(sum);

my $comp_path = shift
  or die "usage: perl run_timed.pl comp_path";

system("rm -f $comp_path/*.s");

my @bfs = glob "$comp_path/prog-*";

my %timings = ();

foreach my $f (@bfs) {
    print "run: $f\n";
    my $start = [gettimeofday];
    system("./$f < check/input.dat > /dev/null");
    my $end = [gettimeofday];
    $timings{$f} = tv_interval $start, $end;
}

my $total = sum (values %timings);

print Dumper(\%timings);

print "Total: $total\n";
