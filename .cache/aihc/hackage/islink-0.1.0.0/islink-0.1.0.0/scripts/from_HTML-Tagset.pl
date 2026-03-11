#!/usr/bin/perl
use strict;
use warnings;
use HTML::Tagset;

while (my ($tag,$attrs)=each %HTML::Tagset::linkElements) {
	for my $attr (@$attrs) {
		print "$tag $attr\n";
	}
}
