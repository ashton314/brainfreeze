#!/usr/bin/env perl
use strict;
use warnings;

my $infile = shift
  or die "Usage: perl $0 <prog.b>\n";

$infile =~ /([^.\/]*).bf?$/
  or die "Benchmark file doesn't look like a bf file.";

my $outfile = $1;

system("racket native.rkt $infile > $outfile.s");
system("clang $outfile.s -o $outfile");
print "Compiled. Running...\n";
system("./$outfile");
print "arm_runner.pl finished."
