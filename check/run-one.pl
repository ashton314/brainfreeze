#!/usr/bin/env perl
use strict;
use warnings;

my $bench = shift
  or die "Usage: run-one.pl <benchmark-name>";

$bench =~ /^prog-([0-9]+).b$/
  or die "Benchmark must be of form prog-([0-9]+).b";

my $bench_num = $1;

system("racket ../native.rkt $bench > prog.s 2>/dev/null");
system("clang prog.s -O -o bf");
system("./bf < input.dat > output.dat 2>/dev/null");

system("diff output.dat output-$bench_num.dat");
