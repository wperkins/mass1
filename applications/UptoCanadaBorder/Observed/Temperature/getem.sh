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
# Last Change: 2018-11-16 11:34:13 d3g096
# -------------------------------------------------------------

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

set -xue

start="01/01/1998"
end="01/01/2020"

monitors="GCGW CHQW WELW RRDW RIGW WANW PRXW PAQW MCNA"
for m in $monitors; do
    srcflag="-w"
    perl $perlinc $massbc $srcflag -q -T -0 -o "${m}_Temperature.dat" $m $start $end
done


