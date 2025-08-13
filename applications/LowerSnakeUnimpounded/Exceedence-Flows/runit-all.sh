#! /bin/sh
# -------------------------------------------------------------
# file: runit-all.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: 2017-10-25 15:08:33 d3g096
# -------------------------------------------------------------

set -x

model=${MASS1-/net/flophouse/files0/perksoft/linux64/mass1/bin/mass1}
pcts="10 50 90"

for p in $pcts; do
    cp mass1_${p}pct.cfg mass1.cfg
    $model
    cp profile1.out profile1-${p}pct.out
    tail -n 342 profile1-${p}pct.out > profile1-${p}pct.last
    for i in ts[0-9]*[0-9].out; do
        n=`expr "$i" : '\(.*\)\.out'`-${p}pct.out
        mv $i $n
    done
done
