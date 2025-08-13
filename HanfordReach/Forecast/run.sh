#! /bin/bash
# -------------------------------------------------------------
# file: run.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 27, 2011 by William A. Perkins
# Last Change: 2017-11-30 13:39:20 d3g096
# -------------------------------------------------------------

# ONLY on FLOPHOUSE!

set -ue

PATH=/usr/local/bin:$PATH
export path

MODEL="/net/flophouse/files0/perksoft/linux64/mass1/bin/mass1"
export MODEL

GDFONTPATH=/usr/share/fonts/liberation
export GDFONTPATH

# cd /home/d3g096/Projects/GrantPUD/HanfordReachForecast/forecast/Forecast
exec 2>&1
exec > run.log

python ./runmass1.py 2>&1
for i in *.eps; do
    png=`expr "$i" : '\(.*\)\.eps' `.png
    convert -density 144 -background white -flatten \
        $i $png
done
# cp results.txt /projects/hanford_forecast/current/mass1-current.csv
# cp q???.png e???.png tw???.png /projects/hanford_forecast/current

