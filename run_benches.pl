#!/usr/bin/env perl
use strict;
use warnings;

my @olevels = (0, 1, 2, 3, 4);

my $comp_path = shift
  or die "usage: perl run_benches.pl comp_path";

system("rm -f $comp_path/*.s");

my @bfs = glob "$comp_path/*.b*";

foreach my $f (@bfs) {
  $f =~ /([^\/]+)\.b.?$/
    or next;

  my $f_base = $1;

  foreach my $olevel (@olevels) {
    print "Compiling $f to $comp_path/$f_base-o$olevel.s...\n";
    system("racket native.rkt -o $olevel $f > $comp_path/$f_base-o$olevel.s");
    system("clang $comp_path/$f_base-o$olevel.s -o $comp_path/$f_base-o$olevel");
  }
}

