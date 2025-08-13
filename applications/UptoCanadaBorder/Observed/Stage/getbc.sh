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
# Last Change: 2018-05-25 13:52:55 d3g096
# -------------------------------------------------------------

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

set -xue

start="01/01/1998"
end="01/01/2020"

dams="GCL CHJ WEL RRH RIS WAN PRD IHR"

for d in $dams; do
    srcflag="-w"
    perl $perlinc $massbc $srcflag -T -0 -o "${d}_TWE.dat" $d $start $end
done
