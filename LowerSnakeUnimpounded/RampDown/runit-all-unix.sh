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
# Last Change: 2018-03-02 07:18:00 d3g096
# -------------------------------------------------------------

set -x

                                
model="${MASS1-mass1}"
flows="200 50 20"


for q in $flows; do
    cp mass1-${q}kcfs.cfg mass1.cfg
    $model
    cp profile1.out profile1-${q}kcfs.out
    tail -n 342 profile1-${q}kcfs.out > profile1-${q}kcfs.last
    for i in ts[0-9]*[0-9].out; do
        n=`expr "$i" : '\(.*\)\.out'`-${q}kcfs.out
        mv $i $n
    done
done
