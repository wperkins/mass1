#!/bin/sh
# -------------------------------------------------------------
# file: getbc.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created May 24, 2018 by William A. Perkins
# Last Change: 2018-11-19 11:48:28 d3g096
# -------------------------------------------------------------

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

set -xue

start="01/01/1998"
end="01/01/2020"

dams="GCL CHJ WEL RRH RIS WAN PRD MCN"

for d in $dams; do
    srcflag="-w"
    perl $perlinc $massbc $srcflag -F -0 -o "${d}_FBE.dat" $d $start $end
done
