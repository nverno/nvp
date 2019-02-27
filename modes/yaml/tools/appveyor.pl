#!/usr/bin/perl -w

use strict;
use utf8;
use warnings;

use LWP::Simple;
use Net::HTTPS;
use YAML::XS;

my $url = 'https://raw.githubusercontent.com/appveyor/website/master/' .
  'src/docs/appveyor-yml.md';

getstore($url, 'appveyor-yml.md');
