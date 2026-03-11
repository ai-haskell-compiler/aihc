#!/usr/bin/perl
use strict;
use warnings;

sub decodeATTLIST {
	my ($tag,$attrs)=@_;
	$tag=lc($tag);
	if ($tag=~/^\((.+)\)$/) {
		my @tags=split /\|/,$1;
		decodeATTLIST($_,$attrs) for @tags;
		return;
	}
	my @attrs=split /\n/,$attrs;
	for my $s (@attrs) {
		if (my ($attr)=$s=~/\s*(\S+)\s+(?:%URI;|%URL )/) {
			print "$tag $attr\n";
		}
	}
}

my $contents=join '',<>;
while ($contents=~/<!ATTLIST +(\S+) *(?:- -.*\n)?([^>]+)>/g) {
	decodeATTLIST($1,$2);
}
