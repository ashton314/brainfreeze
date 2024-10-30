#!/usr/bin/env perl
use strict;
use warnings;

my $bench_path = shift
  or die "usage: perl compile_all.pl bench_path/ comp_path/";
my $comp_path = shift
  or die "usage: perl compile_all.pl bench_path/ comp_path/";

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
