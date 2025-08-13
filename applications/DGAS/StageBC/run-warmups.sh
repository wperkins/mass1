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
# Last Change: 2017-03-01 12:03:02 d3g096
# -------------------------------------------------------------
set -x
set -e

model="${MASS1-$HOME/Projects/MASS1/src/mass1/mass1}"
years="1994 1996 1997"

for year in $years; do

    rm -f mass1.cfg
    sed -e 's/@YEAR@/'$year'/' mass1-warmup-year.cfg > mass1.cfg
    $model 
    # rm -f *.out
done


