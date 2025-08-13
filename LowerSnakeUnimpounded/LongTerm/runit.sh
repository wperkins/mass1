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
# Last Change: Fri Jan  8 15:15:57 1999 by William A. Perkins <perk@erebus.pnl.gov>
# -------------------------------------------------------------

set -x
set -e


model=/net/flophouse/files0/perksoft/linux64/mass1/bin/mass1

cases='1963'

for case in $cases; do
    cp mass1-${case}yr.cfg mass1.cfg
    $model > /dev/null 
    mv profile1.out profile1-${case}.out
    for i in ts[0-9]*[0-9].out status.out; do
        n=`expr "$i" : '\(.*\)\.out'`-${case}.out
        mv $i $n
    done
    rm output.out
done

