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
# Last Change: 2017-03-08 07:32:59 d3g096
# -------------------------------------------------------------
set -x
set -e

                                # to trap floating point errors on SGI

model="${MASS1-$HOME/Projects/MASS1/src/mass1/mass1}"
years="1994 1996 1997"

for year in $years; do
    cp mass1-last-$year.cfg mass1.cfg
    rm -f *.out
    $model 
    sh forebay_plot.sh $year
    sh latflow_plot.sh $year
    rm -f mass1.cfg
done


