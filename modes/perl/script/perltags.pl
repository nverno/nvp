#!/usr/bin/perl -w
# perltags.pl --- Create or update TAGS
# Version: v 0.0 2007/02/14 17:06:42
# Author: Ye Wenbin <wenbinye@163.com>

use strict;
use warnings;
use Getopt::Long qw(:config no_ignore_case pass_through);
use File::Temp qw/:POSIX/;
use Pod::Usage;

my $mv    = "mv";
my $etags = "etags";

my ($exclude, $recursive, $update, $pattern, $language, $tmpfile, $file);

GetOptions(
  "exclude=s"  => \$exclude,
  "R"          => \$recursive,
  "update"     => \$update,
  "pattern=s"  => \$pattern,
  "file=s"     => \$file,
  "language=s" => \$language,
);

my %lang_pat = (
  'elisp' => '\.el$',
  'perl'  => '\.p[ml]$',
  'c'     => '\.(cpp|c|h)$',
);

if ($language) {
  if (!exists $lang_pat{$language}) {
    die "Unknown language: $language. Only support:\n"
      . join("\n", sort keys %lang_pat);
  }
  $pattern = $lang_pat{$language};
}

unless ($exclude || $pattern || $file) {
  warn "No file or pattern given!\n";
  pod2usage();
}

my $dir = ".";

# Backup TAGS
if (-e "TAGS") {
  $tmpfile = tmpnam();
  move("TAGS", $tmpfile) or die "Can't move 'TAGS' to $tmpfile: $!";
}

# Create TAGS for file
if ($pattern || $exclude) {
  unless ($pattern) {
    $pattern = "";
  }
  if ($recursive) {
    require File::Find;
    File::Find::find(
      { no_chdir => 1,
        wanted   => sub {
          return unless -f $_;
          if (/$pattern/) {
            return if defined $exclude && /$exclude/;
            etags("-a", $_, @ARGV)
              or die "Can't create tags file: $!";
          }
        },
      },
      $dir
    );
  } else {
    opendir(DIR, $dir) or die "Can't open directory .: $!";
    foreach (readdir DIR) {
      next unless -f $_;
      if (/$pattern/) {
        next if defined $exclude && /$exclude/;
        etags("-a", $_, @ARGV)
          or die "Can't create tags file: $!";
      }
    }
  }
} else {
  etags("-a", $file, @ARGV)
    or die "Can't create tags file: $!";
}

# update TAGS
if ($update && $tmpfile) {
  if (-e "TAGS") {
    my $new = replace_tag($tmpfile, "TAGS");
    move($new, "TAGS") or die "Can't move $new to 'TAGS': $!";
  } else {
    move($tmpfile, "TAGS") or die "Can't move $tmpfile to 'TAGS': $!";
  }
}

if ($tmpfile && -e $tmpfile) {
  unlink($tmpfile) or die "Can't delete $tmpfile: $!";
}

sub etags {
  my ($tags) = @_;
  print "$etags $tags\n";
  system($etags, $tags);
  return $? == 0;
}

sub move {
  my ($old, $new) = @_;
  system($mv, $old, $new);
  return $? == 0;
}

#==========================================================
# Input  : replace_tag($old_file, $new_file)
# Output : a new file merge the the two TAGS file
# Desc   : Replace tags $new_file to $old_file
#         First, read through $new_file to record new files,
#         Then, read $old_file, print it to a temp file, and
#         next when read a file in new files.
#         At last, append $new_file to the temp file, return
#         the temp file name
#==========================================================
sub replace_tag {
  my ($old, $new) = @_;
  my $line;
  open(my $fh, $new) or die "Can't open file $new: $!";

  # find all file contains in the new TAGS file
  my %files;
  while (<$fh>) {
    if (/^\cL/) {    # A new file start here
      $line = <$fh>;
      if ($line =~ /(.*),\d+$/) {
        $files{$1}++;    # add the new file name
      } else {
        die "$new is NOT a valid TAGS file!";
      }
    }
  }
  my ($tmpfh, $tmpfile) = tmpnam();
  open(my $fh2, $old) or die "Can't open file $old: $!";
  while (<$fh2>) {
    last if /^\cL/;
  }
  while (<$fh2>) {
    $line = $_;
    if ($line =~ /(.*),\d+$/) {

      # If found a file in new TAGS file, ignore it
      if (exists $files{$1}) {
        while (<$fh2>) {
          last if /^\cL/;
        }
      } else {
        print $tmpfh "\f\n";
        print $tmpfh $line;
        while (<$fh2>) {
          last if /^\cL/;
          print $tmpfh $_;
        }
      }
    } else {
      die "$old is NOT a valid TAGS file!";
    }
  }

  # Append the new TAGS file
  seek $fh, 0, 0;
  while (<$fh>) {
    print $tmpfh $_;
  }
  close $fh;
  close $fh2;
  close $tmpfh;
  return $tmpfile;
}

__END__

=head1 NAME

perltags.pl - A customized etags

=head1 SYNOPSIS

perltags.pl [-R -u] [-p pattern | -l language [-e exclude]] -f file

   OPTIONS:
      -p pattern
        create etags for file match pattern

      -l language
        set pattern accord to the language. Currently, support elisp,
        perl and c only.

      -R
        recursive search file in current directory

      -e exclude-pattern 
        exclude file that match exclude-pattern

      -u
        update tags for file

=cut
