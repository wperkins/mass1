#! /bin/sh
# -------------------------------------------------------------
# file: runit-all.sh

# This script generates a restart file for each year specified.  These
# can be used to start a simulation with PID dam representations.
# Generated hotstarts are named 'hotstart-YYYY.dat' where the
# simulation date/time is 01-01-YYYY 00:00:00.
                                
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: 2018-02-07 07:26:59 d3g096
# -------------------------------------------------------------

model="${MASS1-mass1}"

set -xue

y=1999
yend=2016

while [ "$y" -le "$yend" ]; do
    y1=`expr "$y" - 1`
    rm -f mass1.cfg
    sed -e "s/@YEAR@/$y1/g" \
        -e "s/@NEXT@/$y/g" \
        mass1-warmup-year.cfg > mass1.cfg
    $model
    y=`expr "$y" + 1`
done



