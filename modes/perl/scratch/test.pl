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

# Align
my %h                     =  (
  kadksfkskfs1            => 1,
  kas2                    => 2,
  kasdkfasdfkasdkfkasdfk3 => 3,
 );
