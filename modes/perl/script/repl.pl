#!/usr/bin/perl -w
use strict;
use warnings;
use utf8;
use 5.010;

use Reply;
Reply->new(config => "$ENV{HOME}/.replyrc")->run;
