#!/usr/bin/perl -w

use strict;

my @bfs = glob "prog-*.b";

# when the run() function is called, you should interpret, or else
# compile and run, the BF file that is named by the function
# argument. it should read from input.dat and write to output.dat,
# which are both files in the current directory. output.dat is a new
# file that should be created (overwriting any existing file by that
# name) and input.dat is a file that you should assume already exists

sub run1($) {
    (my $f) = @_;
    system("../bf-interp $f < input.dat > output.dat");
}

sub run2($) {
    (my $f) = @_;
    system("../bf-asm < $f");
    system("clang prog.s ../bf-main.c -O -o bf");
    system("./bf < input.dat > output.dat 2>/dev/null");
}

my $success = 0;
my $fail = 0;

foreach my $f (@bfs) {
    print "$f\n";
    die unless $f =~ /^prog-([0-9]+).b$/;
    my $num = $1;
    run2($f);
    my $res = system("diff output.dat output-$num.dat");
    if ($res == 0) {
	++$success;
    print("\tpassed.\n");
    } else {
	++$fail;
	print("\tfailed.\n");
    }
}
print("\n\n$fail failures, $success successes\n\n");
