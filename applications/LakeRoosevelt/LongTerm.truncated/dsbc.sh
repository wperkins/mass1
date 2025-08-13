#!/bin/sh


# Generate downstream boundary condition files for MASS2


set -xue

startyr=1995
endyr=2015
# endyr=1996
modeldir="${MASS1-$HOME/Projects/MASS1/src/mass1/scripts}"
massbc="$modeldir/mass1bc"

y=$startyr
lasty=`expr $y - 1`

dsfiles=""

while [ $y -le $endyr ]; do
    out="downstream-$y.dat"
    gunzip -c $y/ts321.out.$y.gz | \
        "$massbc" -f wselev -o "$out"
    y=`expr $y + 1`
    dsfiles="$dsfiles $out"
done


set $dsfiles
first=1
out="downstream-all.dat"
cp /dev/null "$out"
while [ -n "$*" ]; do
    tail +"$first" $1 >> "$out"
    shift
    first=3
done
