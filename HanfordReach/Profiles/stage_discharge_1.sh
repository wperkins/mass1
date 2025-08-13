#! /bin/sh
# -------------------------------------------------------------
# file: stage_discharge.sh
#
# Generate data and plots of stage/discharge at 10km intervals in the
# Hanford Reacm
#
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 23, 2012 by William A. Perkins
# Last Change: 2022-06-10 11:10:20 d3g096
# -------------------------------------------------------------

set -xue

python=python
#python=python26
extract="/opt/local/share/mass1/profile_extract.py"
extract="/home/d3g096/Projects/MASS1/src/mass1-dhsvm/scripts/profile_extract.py"
profile="profile1.out"

km=553.0
#km=593.0
#km=590.0
#km=583.5

rm=`echo "${km}*0.62137119" | bc `
"$python" "$extract" --river-mile="$rm" "$profile" | \
    awk 'NR > 1 { print; } {next;}' > "stage_discharge_${km}.dat"
sed -e "s/@KM@/$km/g" stage_discharge.gp | gnuplot > "stage_discharge_${km}.eps"
convert -density 200 -alpha off "stage_discharge_${km}.eps" "stage_discharge_${km}.png"

