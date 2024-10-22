#!/usr/bin/env perl
use strict;
use warnings;

my $bench = shift
  or die "Usage: run-one.pl <benchmark-name>";

$bench =~ /^prog-([0-9]+).b$/
  or die "Benchmark must be of form prog-([0-9]+).b";

my $bench_num = $1;

# system("racket ../differ.rkt $bench > prog_diff.s 2>/dev/null");
# system("clang prog_diff.s -O -o bf_diff");
# system("./bf_diff < i1.dat > output_diff.dat 2>/dev/null");

system("racket ../native.rkt $bench > prog.s");
system("clang prog.s -O -o bf");
system("./bf < input.dat > output.dat 2>/dev/null");

# system("diff -s output.dat output_diff.dat");
system("diff output.dat output-$bench_num.dat");
