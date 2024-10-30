#!/usr/bin/env perl
use strict;
use warnings;

my $bench_path = shift
  or die "usage: perl timed_check bench_path/ comp_path/ results_file.txt";
my $comp_path = shift
  or die "usage: perl timed_check bench_path/ comp_path/ results_file.txt";

my @bfs = glob "$bench_path/prog-*.b";

unless (-d $comp_path) {
  mkdir $comp_path;
}

foreach my $f (@bfs) {
  print "Compiling $f...";
  die unless $f =~ /prog-([0-9]+).b$/;
  my $num = $1;

  system("racket native.rkt $f > $comp_path/prog-$num.s 2> /dev/null");
  system("clang $comp_path/prog-$num.s -o $comp_path/prog-$num 2> /dev/null");
  print "done.\n";
}
