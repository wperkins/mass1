#! /bin/sh
# -------------------------------------------------------------
# file: run-by-year.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October  4, 1999 by William A. Perkins
# Last Change: 2017-03-24 14:00:53 d3g096
# -------------------------------------------------------------
set -e
set -x

years="1994 1996 1997"

model=${MODEL-$HOME/Projects/MASS1/src/mass1/mass1}

for year in $years; do

                                # generate necessary file lists:
                                # lateral inflow

    sed -e 's/@YEAR@/'$year'/g' < latflow-files-year.dat > latflow-files-${year}.dat
    
                                # run the season

    rm -f mass1.cfg
    sed -e 's/@YEAR@/'$year'/g' < mass1-run-year.cfg > mass1.cfg
    rm -f *.out
    $model

                                # save time-series output

    for i in ts*.out; do
        mv -f $i "${i}.$year"
    done

done

sh plottw.sh | gnuplot > plottw.ps

