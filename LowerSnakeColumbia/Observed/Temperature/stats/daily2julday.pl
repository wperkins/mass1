#! /usr/unsupported/bin/perl
# -*- mode: cperl -*-
# -------------------------------------------------------------
# file: daily2julday.pl
# This script takes the daily statistics and changes the date to
# julian day.  It puts an empty line between years for plotting straw
# broom plots.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created May 31, 2002 by William A. Perkins
# Last Change: 2018-03-06 10:49:52 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

use strict;
use Date::Manip;

# -------------------------------------------------------------
#  variable initialization
# -------------------------------------------------------------
my $program;
($program = $0) =~ s/.*\///;
my $usage = "usage: $program";

Date::Manip::Date_Init('SetDate=now,UTC');

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

my $lastyr = undef;
while (<>) {
  if (/^#/) {
    print; next;
  }
  chop;
  my @fld = split;
  my $date = shift @fld;
  $date =~ s/-/\//g;
  $date = Date::Manip::ParseDate($date);
  my $jday = Date::Manip::UnixDate($date, "%j");
  my $year = Date::Manip::UnixDate($date, "%Y");
  $lastyr = $year unless($lastyr);
  printf("\n") if ($year != $lastyr);
  printf("%5d %5d %s\n", $year, $jday, join(' ', @fld));
  $lastyr = $year;
}
