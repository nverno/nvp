#!/usr/bin/perl -w

use strict;
use utf8;
use warnings;

use Data::Printer;

print @INC;

for (my $var = 0; $var < 10; $var++) {
  p $var;                       # DEBUG
  print $var
}
