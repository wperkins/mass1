#!/usr/bin/env python2
# -*- mode: python; py-which-shell: "python2";-*-
# -------------------------------------------------------------
# file: extract-travel-time.py
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  7, 2020 by  William Perkins 
# Last Change: 2020-07-07 09:16:39 d3g096
# -------------------------------------------------------------

# RCS ID: $Id$

import sys, os, stat
from optparse import OptionParser
from datetime import *
from time import *
import numpy

# -------------------------------------------------------------
# string2date
# -------------------------------------------------------------
def string2date(s):
    lt = strptime(s, "%m-%d-%Y %H:%M:%S")
    d = datetime(lt.tm_year, lt.tm_mon, lt.tm_mday, lt.tm_hour, lt.tm_min, lt.tm_sec, 0)
    return d

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
program = os.path.basename(sys.argv[0])

release_date = "02-07-2001 00:00:00"
datetime0 = string2date(release_date)

# -------------------------------------------------------------
# handle command line
# -------------------------------------------------------------
usage = "Usage: %prog [options] < ts###.out"
parser = OptionParser(usage=usage)

parser.add_option("-v", "--verbose",
                  dest="verbose", action="store_true", default=False,
                  help="show what's going on")

parser.add_option("-o", "--output", type="string",
                  dest="output", action="store")

(options, args) = parser.parse_args()

doverbose = options.verbose

if options.output:
    output = open(options.output, mode='w')
else:
    output = sys.stdout

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

tseries = []
hrseries = []

for line in sys.stdin:
    tmp = line.strip()
    tmpfld = tmp.split('#')
    if len(tmpfld[0]) <= 0:
        continue
    fld = tmpfld[0].split()
    thedatetime = string2date(fld[0] + " " + fld[1])

    datediff = thedatetime - datetime0
    hrdiff = datediff.total_seconds()/3600.0

    hrseries.append(hrdiff)
    tseries.append(float(fld[8]))
    
    # sys.stderr.write("%s %s %5.1f %5.2f\n" % (fld[0], fld[1], hrdiff, t))

nt = numpy.array(tseries)
tmin = numpy.min(nt)
tmax = numpy.max(nt)
tmid = 0.5*(tmin + tmax)

idx = numpy.max(numpy.where(nt < tmid))
tt = (tmid - tseries[idx])/(tseries[idx+1] - tseries[idx])*(hrseries[idx+1] - hrseries[idx]) + hrseries[idx]
print idx, hrseries[idx], tseries[idx], tmid, tseries[idx+1], tt


