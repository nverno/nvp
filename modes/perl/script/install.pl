#!/usr/bin/perl -w
use strict;
use utf8;
use warnings;
use 5.010;

local $ENV{PATH} = "/home/nverno/perl5/bin:$ENV{PATH}";
print "Have cpanm: " . (system("which cpanm") == 0 ? "yes" : "no") . "\n";

#--- Modules ---------------------------------------------------------
# General:
# - Task::Kensho::Toolchain (cpanm, perlbrew, carton, local::lib, etc)
# - Task::Kensho::Testing
# - Task::Kensho::CLI (cli tools)
# - Task::Kensho::Config
# - Task::Kensho::Execptions
# - Task::Kensho::ModuleDev (dev helpers: critic, tidy, tiny, etc)

# REPL setup:
# - Term::ReadLine::Gnu
# - Reply
# - Devel::REPL (another REPL - re.pl)

my @mods = (
  'Task::Kensho::Toolchain',    # cpanm, perlbrew, carton, local::lib, etc
  'Task::Kensho::Testing',
  'Task::Kensho::CLI',          # cli tools
  'Task::Kensho::Config',
  'Task::Kensho::Exceptions',
  'Task::Kensho::ModuleDev',    # dev helpers: critic, tidy, tiny, etc
  ## REPL related
  'Term::ReadLine',
  'Term::ReadLine::Gnu',
  'Reply',
  'Proc::InvokeEditor',
  'Carp::Always',
  'YAML::Shell'                 # Interactive yaml shell
);

my @replmods = (
  'Term::ReadLine',
  'Term::ReadLine::Gnu',
  'Reply',
  'App::Nopaste',               # biggun
  'Data::Dump',
  'Proc::InvokeEditor',
  'Carp::Always',
);

foreach my $x (@mods) {
  print "Installing $x\n";
  system("cpanm $x");
}
