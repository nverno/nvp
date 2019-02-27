#!/usr/bin/perl -w

use strict;
use utf8;
use warnings;
use 5.010;
use LWP::Simple;

my @urls = (
  'http://yaml.org/spec/1.2/spec.pdf',
  'https://raw.githubusercontent.com/appveyor/website/master/' .
    'src/docs/appveyor-yml.md'
  );

foreach (@urls) {
  print split(/\//, $_)
}
