#!/usr/bin/env perl
use strict;
use warnings;

use Time::HiRes qw(gettimeofday tv_interval);
use Data::Dumper;
use List::Util qw(min);

my @olevels = (0, 1, 2, 3, 4);
my @files = qw(long loopremove mandel serptri twinkle hello hanoi bench deadcodetest bottles);

my %timings = ();

foreach my $f (@files) {
  foreach my $o (@olevels) {
    my @this_file = ();
    print "Running $f-$o 10 times: ";
    for my $i (0..9) {
      print ".";
      my $start = [gettimeofday];
      system("./my_bench/$f-o$o > /dev/null");
      my $end = [gettimeofday];
      push @this_file, (tv_interval $start, $end);
    }
    print "\n";

    my $the_min = min @this_file;

    $timings{"$f-o$o"} = $the_min;
  }
}

print Dumper(\%timings);
