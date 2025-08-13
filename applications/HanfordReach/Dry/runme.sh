#!/bin/sh
# -------------------------------------------------------------
# file: runme.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 11, 2017 by William A. Perkins
# Last Change: 2017-07-11 12:48:48 d3g096
# -------------------------------------------------------------

set -xue

python=${PYTHON-python}
tecplot=${TECPLOT-tecplot}
convert=${CONVERT-convert}
gnuplot=${GNUPLOT-gnuplot}

mass1dir="/home/d3g096/Projects/MASS1/src/mass1-dhsvm"
scriptdir="${mass1dir}/scripts"
mass1="${MASS1-${mass1dir}/build/mass1}"

time "$mass1"

"$python" "$scriptdir/profile_tecplot.py" profile1.out > profile1.dat
rm -rf looper-*.png
"$tecplot" -b -p "$scriptdir/xy-looper-png.mcr" stage1.lay
"$convert" -delay 10 -loop 0 looper-*.png HanfordDry.gif
"$gnuplot" discharge.gp > discharge.eps

