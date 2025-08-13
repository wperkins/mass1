#! /bin/sh
# -------------------------------------------------------------
# file: plots.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Instituteget
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June  8, 2001 by William A. Perkins
# Last Change: 2025-06-10 08:50:31 d3g096
# -------------------------------------------------------------

for file in *-FBE.dat Tidal-Stage.dat; do
    out=`expr "$file" : '\(.*\)\.dat'`
    out=${out}.ps
    title=`head -1 $file | sed -e 's/# *//'`
    cat <<EOF 
set term postscript landscape color dashed "Helvetica" 14
set output "$out"
set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'
set yrange [*:*]
set xrange ['01-01-2008 00:00:00' : '07-01-2025 00:00:00']
set format x "%m-%d\n%Y"
set ylabel 'Elevation, feet'
set title '$title'
plot '$file' using 1:3 title '$file' with lines
EOF
done
